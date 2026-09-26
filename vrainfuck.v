import os

const tape_size = 1 << 25 // 32 MiB
const io_size = 1 << 16 // 64 KiB

enum OpK as u8 {
	nop
	ptr
	add
	put
	get
	jz
	jnz
	zero
	addmul
}

struct Op {
mut:
	op OpK
	a  i32
}

struct Patch {
	at  u32
	tgt u32
}

struct Buf {
mut:
	p   &u8
	pos u32
}

type JitFn = fn ()

fn (mut b Buf) ws(v ...u8) {
	unsafe {
		for i, x in v {
			b.p[b.pos + u32(i)] = x
		}
	}
	b.pos += u32(v.len)
}

fn (mut b Buf) w8(v u8) {
	unsafe {
		b.p[b.pos] = v
	}
	b.pos++
}

fn (mut b Buf) w32(v i32) {
	unsafe {
		p := &u32(b.p + b.pos)
		*p = u32(v)
	}
	b.pos += 4
}

fn (mut b Buf) w64(v u64) {
	unsafe {
		p := &u64(b.p + b.pos)
		*p = v
	}
	b.pos += 8
}

fn (mut b Buf) mabs(rex u8, opc u8, v voidptr) {
	b.w8(rex)
	b.w8(opc)
	b.w64(u64(v))
}

fn (mut b Buf) call_to(t u32) { // call rel32
	b.w8(0xE8)
	b.w32(i32(i64(t) - i64(b.pos) - 4))
}

fn (mut b Buf) patch(at u32, t u32) {
	unsafe {
		p := &i32(b.p + at)
		*p = i32(i64(t) - i64(at) - 4)
	}
}

fn flush_run(mut ops []Op, ch u8, n int) {
	if ch == 43 { // '+'
		ops << Op{.add, n}
	} else if ch == 45 { // '-'
		ops << Op{.add, -n}
	} else if ch == 62 { // '>'
		ops << Op{.ptr, n}
	} else if ch == 60 { // '<'
		ops << Op{.ptr, -n}
	} else if ch == 46 { // '.'
		for _ in 0 .. n {
			ops << Op{.put, 0}
		}
	} else { // ','
		for _ in 0 .. n {
			ops << Op{.get, 0}
		}
	}
}

fn compile(src string) &u8 {
	mut ops := []Op{cap: src.len + 8}
	mut stack := []int{cap: 256}
	mut ch := u8(0)
	mut run := 0
	for i in 0 .. src.len {
		c := src[i]
		match c {
			91, 93 { // [ ]
				if run > 0 {
					flush_run(mut ops, ch, run)
					run = 0
				}
				ch = 0
				if c == 91 {
					ops << Op{.jz, 0}
					stack << ops.len - 1
				} else {
					if stack.len == 0 {
						eprintln('bf: unmatched ]')
						exit(1)
					}
					j := stack.pop()
					ops[j].a = ops.len + 1 // jzは ] の次へ
					ops << Op{.jnz, j + 1} // jnzは [ の次へ
				}
			}
			43, 45, 60, 62, 44, 46 { // + - < > , .
				if c == ch {
					run++
				} else {
					if run > 0 {
						flush_run(mut ops, ch, run)
					}
					ch = c
					run = 1
				}
			}
			else {}
		}
	}
	if run > 0 {
		flush_run(mut ops, ch, run)
	}
	if stack.len > 0 {
		eprintln('bf: unmatched [')
		exit(1)
	}

	mut i := 0
	for i < ops.len {
		if ops[i].op == .jz {
			if i + 2 < ops.len && ops[i].a == i + 3 && ops[i + 2].op == .jnz
				&& ops[i + 2].a == i + 1 && ops[i + 1].op == .add && ops[i + 1].a % 2 != 0 {
				ops[i] = Op{.zero, 0}
				ops[i + 1] = Op{.nop, 0}
				ops[i + 2] = Op{.nop, 0}
				i += 3
				continue
			}
			if i + 5 < ops.len && ops[i].a == i + 6 && ops[i + 5].op == .jnz
				&& ops[i + 5].a == i + 1 {
				mut k := 0
				mut m := 0
				if ops[i + 1].op == .add && ops[i + 1].a == -1 && ops[i + 2].op == .ptr
					&& ops[i + 3].op == .add && ops[i + 4].op == .ptr
					&& ops[i + 4].a == -ops[i + 2].a {
					k = ops[i + 2].a // [->k m+ <-k]
					m = ops[i + 3].a
				} else if ops[i + 1].op == .ptr && ops[i + 2].op == .add && ops[i + 3].op == .ptr
					&& ops[i + 3].a == -ops[i + 1].a && ops[i + 4].op == .add && ops[i + 4].a == -1 {
					k = ops[i + 1].a // [<-k m+ >k -]
					m = ops[i + 2].a
				} else {
					i++
					continue
				}
				if k >= -0x7FFFFF && k <= 0x7FFFFF {
					ops[i] = Op{.addmul, i32((u32(k) & 0xFFFFFF) | (u32(m & 0xFF) << 24))}
					for j in i + 1 .. i + 6 {
						ops[j] = Op{.nop, 0}
					}
					i += 6
					continue
				}
			}
		}
		i++
	}

	tape := unsafe { &u8(C.calloc(tape_size, 1)) }
	outb := unsafe { &u8(C.malloc(io_size)) }
	inb := unsafe { &u8(C.malloc(io_size)) }
	mut code := unsafe {
		&u8(C.mmap(nil, usize(ops.len * 32 + 1024), C.PROT_READ | C.PROT_WRITE,
			C.MAP_PRIVATE | C.MAP_ANONYMOUS, -1, 0))
	}
	if unsafe { isize(voidptr(code)) == -1 } {
		eprintln('bf: mmap failed')
		exit(1)
	}
	mut jb := Buf{
		p:   code
		pos: 0
	}

	jb.ws(0x53, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41, 0x57) // push rbx,r12,r13,r14,r15
	unsafe {
		jb.mabs(0x48, 0xBB, voidptr(tape + tape_size / 2)) // mov rbx, テープ中央
		jb.mabs(0x49, 0xBC, voidptr(outb)) // mov r12, OUT
		jb.mabs(0x49, 0xBD, voidptr(outb + io_size)) // mov r13, OUT_END
		jb.mabs(0x49, 0xBE, voidptr(inb + io_size)) // mov r14, IN_END (入力空)
		jb.mabs(0x49, 0xBF, voidptr(inb + io_size)) // mov r15, IN_END
	}
	jb.w8(0xE9)
	jmp_over := jb.pos
	jb.w32(0)

	flush_at := jb.pos
	jb.ws(0x50, 0x51, 0x52, 0x56, 0x57, 0x41, 0x53) // push rax,rcx,rdx,rsi,rdi,r11
	unsafe {
		jb.mabs(0x48, 0xBE, voidptr(outb)) // mov rsi, OUT
	}
	wloop := jb.pos
	// mov eax,1(SYS_write); mov edi,1; syscall; test rax,rax; jle +8; add rsi,rax; sub rdx,rax; jnz wloop
	jb.ws(0xB8, 1, 0, 0, 0, 0xBF, 1, 0, 0, 0, 0x0F, 0x05, 0x48, 0x85, 0xC0, 0x7E, 8, 0x48, 0x01,
		0xC6, 0x48, 0x29, 0xC2, 0x75)
	jb.w8(u8(i32(i64(wloop) - i64(jb.pos) - 1)))
	jb.ws(0x41, 0x5B, 0x5F, 0x5E, 0x5A, 0x59, 0x58) // pop r11,rdi,rsi,rdx,rcx,rax
	unsafe {
		jb.mabs(0x49, 0xBC, voidptr(outb)) // mov r12, OUT
	}
	jb.w8(0xC3) // ret

	refill_at := jb.pos
	// push rcx,rdx,rsi,rdi,r11; xor eax,eax(SYS_read); xor edi,edi(stdin)
	jb.ws(0x51, 0x52, 0x56, 0x57, 0x41, 0x53, 0x31, 0xC0, 0x31, 0xFF)
	unsafe {
		jb.mabs(0x48, 0xBE, voidptr(inb)) // mov rsi, IN
	}
	// mov edx,io_size; syscall; test rax,rax; jle eof(+32)
	jb.ws(0xBA, u8(io_size), u8(io_size >> 8), u8(io_size >> 16), u8(io_size >> 24), 0x0F, 0x05,
		0x48, 0x85, 0xC0, 0x7E, 32)
	unsafe {
		jb.mabs(0x49, 0xBF, voidptr(inb)) // mov r15, IN
	}
	jb.ws(0x49, 0x01, 0xC7) // add r15, rax (=IN+n)
	unsafe {
		jb.mabs(0x49, 0xBE, voidptr(inb)) // mov r14, IN
	}
	jb.ws(0x41, 0x0F, 0xB6, 0x06, 0x49, 0xFF, 0xC6, 0xEB, 2) // movzx eax,[r14]; inc r14; jmp done
	jb.ws(0x31, 0xC0) // eof: xor eax,eax
	jb.ws(0x41, 0x5B, 0x5F, 0x5E, 0x5A, 0x59, 0xC3) // pop r11,rdi,rsi,rdx,rcx; ret

	jb.patch(jmp_over, jb.pos)
	mut pos_of := []u32{len: ops.len + 1}
	mut patches := []Patch{cap: 16}
	mut fl := u8(0)
	for oi, o in ops {
		pos_of[oi] = jb.pos
		a := o.a
		if o.op == .ptr {
			if a != 0 {
				if a >= -128 && a <= 127 {
					jb.ws(0x48, 0x83, 0xC3, u8(a)) // add rbx, imm8
				} else {
					jb.ws(0x48, 0x81, 0xC3) // add rbx, imm32
					jb.w32(a)
				}
				fl = 0
			}
		} else if o.op == .add {
			m := a & 0xFF
			if m == 1 {
				jb.ws(0xFE, 0x03) // inc byte [rbx]
				fl = 1
			} else if m == 255 {
				jb.ws(0xFE, 0x0B) // dec byte [rbx]
				fl = 1
			} else if m != 0 {
				jb.ws(0x80, 0x03, u8(m)) // add byte [rbx], imm8 (u8ラップ)
				fl = 1
			}
		} else if o.op == .zero {
			jb.ws(0xC6, 0x03, 0x00) // mov byte [rbx], 0
			fl = 2
		} else if o.op == .addmul {
			k := i32(u32(a) << 8) >> 8 // 下位24bit(sign拡張)
			m := u8(u32(a) >> 24) // 乗数 mod 256
			jb.ws(0x0F, 0xB6, 0x03) // movzx eax, [rbx]
			if m != 0 {
				if m != 1 {
					jb.ws(0x6B, 0xC0, m) // imul eax, m
				}
				if k >= -128 && k <= 127 {
					jb.ws(0x00, 0x43, u8(k)) // add [rbx+disp8], al
				} else {
					jb.ws(0x00, 0x83) // add [rbx+disp32], al
					jb.w32(k)
				}
			}
			jb.ws(0xC6, 0x03, 0x00) // mov byte [rbx], 0
			fl = 2
		} else if o.op == .put {
			// movzx eax,[rbx]; mov [r12],al; inc r12; cmp r12,r13; jb +10; mov edx,io_size; call flush
			jb.ws(0x0F, 0xB6, 0x03, 0x41, 0x88, 0x04, 0x24, 0x49, 0xFF, 0xC4, 0x4D, 0x39, 0xEC,
				0x72, 10, 0xBA, u8(io_size), u8(io_size >> 8), u8(io_size >> 16), u8(io_size >> 24))
			jb.call_to(flush_at)
			fl = 0
		} else if o.op == .get {
			// cmp r14,r15; jb +7; call refill; jmp +7; movzx eax,[r14]; inc r14; mov [rbx],al
			jb.ws(0x4D, 0x39, 0xFE, 0x72, 7)
			jb.call_to(refill_at)
			jb.ws(0xEB, 7, 0x41, 0x0F, 0xB6, 0x06, 0x49, 0xFF, 0xC6, 0x88, 0x03)
			fl = 0
		} else if o.op == .jz {
			if fl == 2 {
				jb.w8(0xE9)
				patches << Patch{jb.pos, u32(a)}
				jb.w32(0)
				fl = 0
			} else {
				if fl != 1 {
					jb.ws(0x80, 0x3B, 0x00) // cmp byte [rbx], 0
				}
				jb.ws(0x0F, 0x84) // je rel32
				patches << Patch{jb.pos, u32(a)}
				jb.w32(0)
				fl = 1
			}
		} else if o.op == .jnz {
			if fl == 2 {
				fl = 0
			} else {
				if fl != 1 {
					jb.ws(0x80, 0x3B, 0x00) // cmp byte [rbx], 0
				}
				jb.ws(0x0F, 0x85) // jne rel32
				patches << Patch{jb.pos, u32(a)}
				jb.w32(0)
				fl = 1
			}
		}
	}
	pos_of[ops.len] = jb.pos

	unsafe {
		jb.mabs(0x48, 0xB8, voidptr(outb)) // mov rax, OUT
	}
	jb.ws(0x4C, 0x89, 0xE2, 0x48, 0x29, 0xC2, 0x74, 5) // mov rdx,r12; sub rdx,rax; jz +5
	jb.call_to(flush_at)
	jb.ws(0x41, 0x5F, 0x41, 0x5E, 0x41, 0x5D, 0x41, 0x5C, 0x5B, 0xC3) // pop r15,r14,r13,r12,rbx; ret

	for p in patches {
		jb.patch(p.at, pos_of[p.tgt])
	}
	if unsafe {
		C.mprotect(voidptr(code), usize((jb.pos + 4095) & ~u32(4095)), C.PROT_READ | C.PROT_EXEC) != 0
	} {
		eprintln('bf: mprotect failed')
		exit(1)
	}
	return code
}

fn main() {
	if os.args.len < 2 {
		eprintln('usage: bf FILE.bf [...]')
		exit(1)
	}
	for path in os.args[1..] {
		src := os.read_file(path) or {
			eprintln('bf: cannot open $path')
			exit(1)
		}
		unsafe {
			f := JitFn(voidptr(compile(src)))
			f()
		}
	}
}
