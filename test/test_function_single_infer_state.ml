open Type.Single_type_infer
open Type.Smt_emitter
open Type.Single_exp

let standalone_salsa20_single_infer_state : SingleTypeInfer.t list =
	[
		{
			func_name = "salsa20_words";
			func =
				[
					{
						label = "salsa20_words";
						insts = [
							Jmp ".LFB2";
						]
					};
					{
						label = ".LFB2";
						insts = [
							Push (RegOp R15, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							UInst (Mov, RegOp RCX, RegOp RSI);
							Push (RegOp R14, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), true), None));
							Push (RegOp R13, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), true), None));
							Push (RegOp R12, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), true), None));
							Push (RegOp RBP, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), true), None));
							Push (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), true), None));
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum 8L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp EBP, LdOp(Some (ImmNum 16L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp EDX, LdOp(Some (ImmNum 44L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R8D, LdOp(None, Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R15D, LdOp(Some (ImmNum 4L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R13D, LdOp(Some (ImmNum 20L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R11D, LdOp(Some (ImmNum 36L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum (-44L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), true), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum 12L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp EBX, LdOp(Some (ImmNum 56L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum (-32L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), true), None)), RegOp EDX);
							UInst (Mov, RegOp R14D, RegOp R8D);
							UInst (Mov, RegOp EDX, LdOp(Some (ImmNum 48L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp ESI, LdOp(Some (ImmNum 24L), Some (RSI), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R12D, LdOp(Some (ImmNum 32L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R9D, LdOp(Some (ImmNum 52L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum (-40L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), true), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum 28L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp R10D, RegOp EDX);
							UInst (Mov, RegOp EDX, RegOp EBX);
							UInst (Mov, StOp(Some (ImmNum (-36L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), true), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum 40L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum 60L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum (-24L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), true), None)), ImmOp (ImmNum 10L));
							UInst (Mov, StOp(Some (ImmNum (-28L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), true), None)), RegOp EBP);
							UInst (Mov, StOp(Some (ImmNum (-8L)), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), true), None)), RegOp RCX);
							UInst (Mov, StOp(Some (ImmNum (-20L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), true), None)), RegOp R8D);
							UInst (Mov, RegOp R8D, LdOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)));
							UInst (Mov, StOp(Some (ImmNum (-16L)), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), true), None)), RegOp RDI);
							UInst (Mov, RegOp EDI, RegOp R11D);
							Jmp ".L2";
						]
					};
					{
						label = ".L2";
						insts = [
							UInst (Mov, RegOp EBX, LdOp(Some (ImmNum (-28L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), true), None)));
							UInst (Lea, RegOp EBP, MemOp (None, Some (R14), Some (R10), None));
							UInst (Lea, RegOp R11D, MemOp (Some (ImmNum 0L), Some (R13), Some (R15), None));
							BInst (Rol, RegOp EBP, RegOp EBP, ImmOp (ImmNum 7L));
							BInst (Rol, RegOp R11D, RegOp R11D, ImmOp (ImmNum 7L));
							BInst (Xor, RegOp EBP, RegOp EBP, RegOp EBX);
							BInst (Xor, RegOp R11D, RegOp R11D, RegOp EDI);
							UInst (Lea, RegOp ECX, MemOp (Some (ImmNum 0L), Some (RBP), Some (R14), None));
							BInst (Rol, RegOp ECX, RegOp ECX, ImmOp (ImmNum 9L));
							BInst (Xor, RegOp ECX, RegOp ECX, RegOp R12D);
							UInst (Mov, RegOp R12D, LdOp(Some (ImmNum (-36L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), true), None)));
							UInst (Mov, RegOp EBX, RegOp ECX);
							UInst (Lea, RegOp ECX, MemOp (Some (ImmNum 0L), Some (RBP), Some (RCX), None));
							BInst (Rol, RegOp ECX, RegOp ECX, ImmOp (ImmNum 13L));
							UInst (Mov, StOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)), RegOp EBX);
							BInst (Xor, RegOp ECX, RegOp ECX, RegOp R10D);
							UInst (Lea, RegOp R10D, MemOp (None, Some (RBX), Some (RCX), None));
							UInst (Mov, StOp(Some (ImmNum (-52L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), true), None)), RegOp ECX);
							UInst (Lea, RegOp ECX, MemOp (None, Some (R11), Some (R13), None));
							BInst (Rol, RegOp ECX, RegOp ECX, ImmOp (ImmNum 9L));
							UInst (Lea, RegOp EBX, MemOp (None, Some (R8), Some (RSI), None));
							BInst (Ror, RegOp R10D, RegOp R10D, ImmOp (ImmNum 14L));
							BInst (Xor, RegOp ECX, RegOp ECX, RegOp R9D);
							BInst (Xor, RegOp R10D, RegOp R10D, RegOp R14D);
							BInst (Rol, RegOp EBX, RegOp EBX, ImmOp (ImmNum 7L));
							UInst (Lea, RegOp R14D, MemOp (None, Some (R11), Some (RCX), None));
							BInst (Xor, RegOp EBX, RegOp EBX, RegOp EDX);
							UInst (Mov, RegOp EDI, RegOp ECX);
							UInst (Mov, StOp(Some (ImmNum (-48L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), true), None)), RegOp ECX);
							BInst (Rol, RegOp R14D, RegOp R14D, ImmOp (ImmNum 13L));
							UInst (Lea, RegOp R9D, MemOp (None, Some (RBX), Some (R8), None));
							BInst (Xor, RegOp R14D, RegOp R14D, RegOp R15D);
							UInst (Mov, RegOp R15D, LdOp(Some (ImmNum (-44L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), true), None)));
							BInst (Rol, RegOp R9D, RegOp R9D, ImmOp (ImmNum 9L));
							BInst (Add, RegOp EDI, RegOp EDI, RegOp R14D);
							BInst (Xor, RegOp R9D, RegOp R9D, RegOp R15D);
							BInst (Ror, RegOp EDI, RegOp EDI, ImmOp (ImmNum 14L));
							UInst (Mov, RegOp R15D, LdOp(Some (ImmNum (-32L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), true), None)));
							BInst (Xor, RegOp EDI, RegOp EDI, RegOp R13D);
							UInst (Lea, RegOp R13D, MemOp (None, Some (RBX), Some (R9), None));
							BInst (Rol, RegOp R13D, RegOp R13D, ImmOp (ImmNum 13L));
							BInst (Xor, RegOp R13D, RegOp R13D, RegOp ESI);
							UInst (Lea, RegOp ESI, MemOp (None, Some (RAX), Some (R15), None));
							UInst (Lea, RegOp EDX, MemOp (None, Some (R9), Some (R13), None));
							BInst (Rol, RegOp ESI, RegOp ESI, ImmOp (ImmNum 7L));
							BInst (Ror, RegOp EDX, RegOp EDX, ImmOp (ImmNum 14L));
							BInst (Xor, RegOp EDX, RegOp EDX, RegOp R8D);
							UInst (Mov, RegOp R8D, LdOp(Some (ImmNum (-40L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), true), None)));
							BInst (Xor, RegOp ESI, RegOp ESI, RegOp R8D);
							UInst (Lea, RegOp R8D, MemOp (None, Some (RSI), Some (RAX), None));
							BInst (Rol, RegOp R8D, RegOp R8D, ImmOp (ImmNum 9L));
							BInst (Xor, RegOp R8D, RegOp R8D, RegOp R12D);
							UInst (Lea, RegOp R12D, MemOp (None, Some (RSI), Some (R8), None));
							BInst (Rol, RegOp R12D, RegOp R12D, ImmOp (ImmNum 13L));
							BInst (Xor, RegOp R12D, RegOp R12D, RegOp R15D);
							UInst (Lea, RegOp R15D, MemOp (None, Some (R10), Some (RSI), None));
							UInst (Lea, RegOp ECX, MemOp (None, Some (R8), Some (R12), None));
							BInst (Ror, RegOp ECX, RegOp ECX, ImmOp (ImmNum 14L));
							BInst (Xor, RegOp ECX, RegOp ECX, RegOp EAX);
							BInst (Rol, RegOp R15D, RegOp R15D, ImmOp (ImmNum 7L));
							BInst (Xor, RegOp R15D, RegOp R15D, RegOp R14D);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R10), Some (R15), None));
							BInst (Rol, RegOp EAX, RegOp EAX, ImmOp (ImmNum 9L));
							UInst (Mov, RegOp R14D, RegOp EAX);
							BInst (Xor, RegOp R14D, RegOp R14D, RegOp R9D);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R15), Some (R14), None));
							UInst (Mov, StOp(Some (ImmNum (-44L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), true), None)), RegOp R14D);
							BInst (Rol, RegOp EAX, RegOp EAX, ImmOp (ImmNum 13L));
							UInst (Mov, RegOp R9D, RegOp EAX);
							BInst (Xor, RegOp R9D, RegOp R9D, RegOp ESI);
							UInst (Lea, RegOp ESI, MemOp (Some (ImmNum 0L), Some (RBP), Some (RDI), None));
							BInst (Rol, RegOp ESI, RegOp ESI, ImmOp (ImmNum 7L));
							BInst (Add, RegOp R14D, RegOp R14D, RegOp R9D);
							UInst (Mov, StOp(Some (ImmNum (-40L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), true), None)), RegOp R9D);
							BInst (Xor, RegOp ESI, RegOp ESI, RegOp R13D);
							BInst (Ror, RegOp R14D, RegOp R14D, ImmOp (ImmNum 14L));
							UInst (Lea, RegOp EAX, MemOp (None, Some (RDI), Some (RSI), None));
							BInst (Xor, RegOp R14D, RegOp R14D, RegOp R10D);
							BInst (Rol, RegOp EAX, RegOp EAX, ImmOp (ImmNum 9L));
							UInst (Mov, RegOp R9D, RegOp EAX);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R11), Some (RDX), None));
							BInst (Xor, RegOp R9D, RegOp R9D, RegOp R8D);
							BInst (Rol, RegOp EAX, RegOp EAX, ImmOp (ImmNum 7L));
							UInst (Lea, RegOp R8D, MemOp (None, Some (RSI), Some (R9), None));
							UInst (Mov, StOp(Some (ImmNum (-36L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), true), None)), RegOp R9D);
							BInst (Rol, RegOp R8D, RegOp R8D, ImmOp (ImmNum 13L));
							UInst (Mov, RegOp R10D, RegOp R8D);
							UInst (Mov, RegOp R8D, RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)));
							BInst (Xor, RegOp R8D, RegOp R8D, RegOp R12D);
							BInst (Xor, RegOp R10D, RegOp R10D, RegOp EBP);
							UInst (Lea, RegOp R12D, MemOp (None, Some (RDX), Some (R8), None));
							UInst (Lea, RegOp R13D, MemOp (None, Some (R9), Some (R10), None));
							UInst (Mov, StOp(Some (ImmNum (-32L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), true), None)), RegOp R8D);
							BInst (Rol, RegOp R12D, RegOp R12D, ImmOp (ImmNum 9L));
							BInst (Ror, RegOp R13D, RegOp R13D, ImmOp (ImmNum 14L));
							UInst (Mov, StOp(Some (ImmNum (-28L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), true), None)), RegOp R10D);
							UInst (Lea, RegOp R10D, MemOp (None, Some (RBX), Some (RCX), None));
							BInst (Xor, RegOp R12D, RegOp R12D, RegOp EAX);
							BInst (Xor, RegOp R13D, RegOp R13D, RegOp EDI);
							BInst (Rol, RegOp R10D, RegOp R10D, ImmOp (ImmNum 7L));
							UInst (Lea, RegOp EDI, MemOp (None, Some (R8), Some (R12), None));
							BInst (Rol, RegOp EDI, RegOp EDI, ImmOp (ImmNum 13L));
							BInst (Xor, RegOp EDI, RegOp EDI, RegOp R11D);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R12), Some (RDI), None));
							BInst (Ror, RegOp EAX, RegOp EAX, ImmOp (ImmNum 14L));
							UInst (Mov, RegOp R8D, RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-48L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), true), None)));
							BInst (Xor, RegOp R8D, RegOp R8D, RegOp EDX);
							UInst (Mov, RegOp EDX, LdOp(Some (ImmNum (-52L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), true), None)));
							BInst (Xor, RegOp R10D, RegOp R10D, RegOp EDX);
							UInst (Lea, RegOp R9D, MemOp (None, Some (RCX), Some (R10), None));
							BInst (Rol, RegOp R9D, RegOp R9D, ImmOp (ImmNum 9L));
							BInst (Xor, RegOp R9D, RegOp R9D, RegOp EAX);
							UInst (Lea, RegOp EDX, MemOp (None, Some (R10), Some (R9), None));
							BInst (Rol, RegOp EDX, RegOp EDX, ImmOp (ImmNum 13L));
							BInst (Xor, RegOp EDX, RegOp EDX, RegOp EBX);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R9), Some (RDX), None));
							BInst (Ror, RegOp EAX, RegOp EAX, ImmOp (ImmNum 14L));
							BInst (Xor, RegOp EAX, RegOp EAX, RegOp ECX);
							BInst (Sub, StOp(Some (ImmNum (-24L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), true), None)), LdOp(Some (ImmNum (-24L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), true), None)), ImmOp (ImmNum 1L));
							Jcond (JNe, ".L2");
							UInst (Mov, StOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)), RegOp R8D);
							UInst (Mov, RegOp R8D, LdOp(Some (ImmNum (-20L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), true), None)));
							UInst (Mov, RegOp R11D, RegOp EDI);
							UInst (Mov, RegOp EBX, RegOp EDX);
							UInst (Mov, RegOp RDI, LdOp(Some (ImmNum (-16L)), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), true), None)));
							UInst (Mov, RegOp RCX, LdOp(Some (ImmNum (-8L)), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), true), None)));
							UInst (Mov, RegOp EDX, RegOp R10D);
							UInst (Mov, RegOp R10D, RegOp EAX);
							UInst (Lea, RegOp EAX, MemOp (None, Some (R14), Some (R8), None));
							UInst (Mov, RegOp EBP, LdOp(Some (ImmNum (-28L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), true), None)));
							UInst (Mov, StOp(None, Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							BInst (Add, RegOp R15D, RegOp R15D, LdOp(Some (ImmNum 4L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 4L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R15D);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-44L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), true), None)));
							BInst (Add, RegOp EAX, RegOp EAX, LdOp(Some (ImmNum 8L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 8L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-40L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), true), None)));
							BInst (Add, RegOp EAX, RegOp EAX, LdOp(Some (ImmNum 12L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 12L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							BInst (Add, RegOp EBP, RegOp EBP, LdOp(Some (ImmNum 16L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 16L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EBP);
							BInst (Add, RegOp R13D, RegOp R13D, LdOp(Some (ImmNum 20L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 20L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R13D);
							BInst (Add, RegOp ESI, RegOp ESI, LdOp(Some (ImmNum 24L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-36L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), true), None)));
							UInst (Mov, StOp(Some (ImmNum 24L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp ESI);
							BInst (Add, RegOp EAX, RegOp EAX, LdOp(Some (ImmNum 28L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 28L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							BInst (Add, RegOp R12D, RegOp R12D, LdOp(Some (ImmNum 32L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 32L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R12D);
							BInst (Add, RegOp R11D, RegOp R11D, LdOp(Some (ImmNum 36L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-56L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), true), None)));
							UInst (Mov, StOp(Some (ImmNum 36L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R11D);
							BInst (Add, RegOp EAX, RegOp EAX, LdOp(Some (ImmNum 40L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 40L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							UInst (Mov, RegOp EAX, LdOp(Some (ImmNum (-32L)), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), true), None)));
							BInst (Add, RegOp EAX, RegOp EAX, LdOp(Some (ImmNum 44L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 44L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EAX);
							BInst (Add, RegOp EDX, RegOp EDX, LdOp(Some (ImmNum 48L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 48L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EDX);
							BInst (Add, RegOp R9D, RegOp R9D, LdOp(Some (ImmNum 52L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 52L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R9D);
							BInst (Add, RegOp EBX, RegOp EBX, LdOp(Some (ImmNum 56L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 56L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp EBX);
							UInst (Mov, RegOp R15D, LdOp(Some (ImmNum 60L), Some (RCX), None, None, 4L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), false), None)));
							BInst (Add, RegOp R15D, RegOp R15D, RegOp R10D);
							UInst (Mov, StOp(Some (ImmNum 60L), Some (RDI), None, None, 4L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp R15D);
							Pop (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), true), None));
							Pop (RegOp RBP, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), true), None));
							Pop (RegOp R12, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), true), None));
							Pop (RegOp R13, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), true), None));
							Pop (RegOp R14, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), true), None));
							Pop (RegOp R15, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							Jmp ".Ret";
						]
					};
					{
						label = ".LFE2";
						insts = [
						]
					};
					{
						label = ".Ret";
						insts = [
						]
					};
				]
				;
			func_type =
				[
					{
						label = "salsa20_words";
						pc = 5;
						reg_type = 
							[
								SingleVar (0);
								SingleVar (1);
								SingleVar (2);
								SingleVar (3);
								SingleVar (4);
								SingleVar (5);
								SingleVar (6);
								SingleVar (7);
								SingleVar (8);
								SingleVar (9);
								SingleVar (10);
								SingleVar (11);
								SingleVar (12);
								SingleVar (13);
								SingleVar (14);
								SingleVar (15);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), RangeConst [], SingleVar (93));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (94));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), RangeConst [], SingleVar (95));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (96));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), RangeConst [], SingleVar (97));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (98));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), RangeConst [], SingleVar (99));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (100));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), RangeConst [], SingleVar (101));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), RangeConst [], SingleVar (102));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (103));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), RangeConst [], SingleVar (104));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (105));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (106));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (107));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (108));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (109));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (110));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleTop);
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleTop);
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L)))], SingleTop);
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFB2";
						pc = 7;
						reg_type = 
							[
								SingleVar (17);
								SingleVar (18);
								SingleVar (19);
								SingleVar (20);
								SingleVar (21);
								SingleVar (22);
								SingleVar (23);
								SingleVar (24);
								SingleVar (25);
								SingleVar (26);
								SingleVar (27);
								SingleVar (28);
								SingleVar (29);
								SingleVar (30);
								SingleVar (31);
								SingleVar (32);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), RangeConst [], SingleVar (111));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (112));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), RangeConst [], SingleVar (113));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (114));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), RangeConst [], SingleVar (115));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (116));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), RangeConst [], SingleVar (117));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (118));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), RangeConst [], SingleVar (119));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), RangeConst [], SingleVar (120));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (121));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), RangeConst [], SingleVar (122));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (123));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (124));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (125));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (126));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (127));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (128));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (33));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (34));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L)))], SingleVar (35));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [21; 23];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L2";
						pc = 47;
						reg_type = 
							[
								SingleVar (36);
								SingleVar (37);
								SingleVar (38);
								SingleVar (39);
								SingleVar (40);
								SingleVar (41);
								SingleVar (42);
								SingleVar (43);
								SingleVar (44);
								SingleVar (45);
								SingleVar (46);
								SingleVar (47);
								SingleVar (48);
								SingleVar (49);
								SingleVar (50);
								SingleVar (51);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), RangeConst [], SingleVar (129));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (130));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), RangeConst [], SingleVar (131));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (132));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), RangeConst [], SingleVar (133));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (134));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), RangeConst [], SingleVar (135));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (136));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), RangeConst [], SingleVar (137));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), RangeConst [], SingleVar (138));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (139));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), RangeConst [], SingleVar (140));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (141));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (142));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (143));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (144));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (145));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (146));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (52));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (53));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L)))], SingleVar (54));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [40; 139; 140];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFE2";
						pc = 224;
						reg_type = 
							[
								SingleVar (55);
								SingleVar (56);
								SingleVar (57);
								SingleVar (58);
								SingleVar (59);
								SingleVar (60);
								SingleVar (61);
								SingleVar (62);
								SingleVar (63);
								SingleVar (64);
								SingleVar (65);
								SingleVar (66);
								SingleVar (67);
								SingleVar (68);
								SingleVar (69);
								SingleVar (70);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), RangeConst [], SingleVar (147));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (148));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), RangeConst [], SingleVar (149));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (150));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), RangeConst [], SingleVar (151));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (152));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), RangeConst [], SingleVar (153));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (154));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), RangeConst [], SingleVar (155));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), RangeConst [], SingleVar (156));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (157));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), RangeConst [], SingleVar (158));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (159));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (160));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (161));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (162));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (163));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (164));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (71));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (72));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L)))], SingleVar (73));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".Ret";
						pc = 225;
						reg_type = 
							[
								SingleVar (74);
								SingleVar (75);
								SingleVar (76);
								SingleVar (77);
								SingleVar (78);
								SingleVar (79);
								SingleVar (80);
								SingleVar (81);
								SingleVar (82);
								SingleVar (83);
								SingleVar (84);
								SingleVar (85);
								SingleVar (86);
								SingleVar (87);
								SingleVar (88);
								SingleVar (89);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), RangeConst [], SingleVar (165));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (166));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), RangeConst [], SingleVar (167));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (168));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), RangeConst [], SingleVar (169));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (170));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), RangeConst [], SingleVar (171));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (172));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), RangeConst [], SingleVar (173));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), RangeConst [], SingleVar (174));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (175));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), RangeConst [], SingleVar (176));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (177));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (178));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (179));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (180));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (181));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (182));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (90));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (91));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L)))], SingleVar (92));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [77; 78; 79; 86; 87; 88; 89; 91; 92];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
				]
				;
			single_subtype =
				[
					{ var_idx = (15, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (14, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (13, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (12, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (5, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (3, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (35, 7); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (32, 7); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (31, 7); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (30, 7); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (29, 7); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (22, 7); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (20, 7); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (7, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (54, 47); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (92, 225); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (91, 225); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (146, 47); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (89, 225); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (145, 47); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (88, 225); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (144, 47); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (87, 225); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (143, 47); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (86, 225); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (142, 47); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (79, 225); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (78, 225); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
					{ var_idx = (141, 47); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (77, 225); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (24, 7); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (140, 47); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (139, 47); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (40, 47); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (6, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (23, 7); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (4, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (21, 7); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
				]
				;
			next_var = SingleTop;
			input_var_set = SingleExp.SingleVarSet.of_list [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
			smt_ctx = SmtEmitter.init_smt_ctx ();
		}
		;
		{
			func_name = "salsa20_block";
			func =
				[
					{
						label = "salsa20_block";
						insts = [
							Jmp ".LFB3";
						]
					};
					{
						label = ".LFB3";
						insts = [
							Push (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							UInst (Mov, RegOp RBX, RegOp RDI);
							BInst (Add, RegOp RSP, RegOp RSP, ImmOp (ImmNum (-128L)));
							UInst (Mov, RegOp RAX, LdOp(None, Some (RSI), None, None, 8L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), false), None)));
							UInst (Lea, RegOp RDI, MemOp (Some (ImmNum 64L), Some (RSP), None, None));
							UInst (Mov, StOp(None, Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), ImmOp (ImmNum 1634760805L));
							UInst (Mov, StOp(Some (ImmNum 4L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RAX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 8L), Some (RSI), None, None, 8L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 20L), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), ImmOp (ImmNum 857760878L));
							UInst (Mov, StOp(Some (ImmNum 12L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RAX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 16L), Some (RSI), None, None, 8L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), false), None)));
							UInst (Mov, StOp(Some (ImmNum 24L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RDX);
							UInst (Mov, StOp(Some (ImmNum 44L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RAX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 24L), Some (RSI), None, None, 8L, (Some (6, (SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), false), None)));
							UInst (Mov, RegOp RSI, RegOp RSP);
							UInst (Mov, StOp(Some (ImmNum 32L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RCX);
							UInst (Mov, StOp(Some (ImmNum 52L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), RegOp RAX);
							UInst (Mov, StOp(Some (ImmNum 40L), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), ImmOp (ImmNum 2036477234L));
							UInst (Mov, StOp(Some (ImmNum 60L), Some (RSP), None, None, 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), false), None)), ImmOp (ImmNum 1797285236L));
							Call "salsa20_words";
							BInst (Xor, RegOp EAX, RegOp EAX, RegOp EAX);
							Jmp ".L7";
						]
					};
					{
						label = ".L7";
						insts = [
							UInst (Mov, RegOp EDX, RegOp EAX);
							UInst (Mov, RegOp ECX, RegOp EAX);
							BInst (Sar, RegOp EDX, RegOp EDX, ImmOp (ImmNum 2L));
							BInst (And, RegOp ECX, RegOp ECX, ImmOp (ImmNum 3L));
							UInst (MovS, RegOp RDX, RegOp EDX);
							BInst (Sal, RegOp ECX, RegOp ECX, ImmOp (ImmNum 3L));
							UInst (Mov, RegOp EDX, LdOp(Some (ImmNum 64L), Some (RSP), Some (RDX), Some (Scale4), 4L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), false), None)));
							BInst (Shr, RegOp EDX, RegOp EDX, RegOp CL);
							UInst (Mov, StOp(None, Some (RBX), Some (RAX), None, 1L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), false), None)), RegOp DL);
							BInst (Add, RegOp RAX, RegOp RAX, ImmOp (ImmNum 1L));
							Cmp (RegOp RAX, ImmOp (ImmNum 64L));
							Jcond (JNe, ".L7");
							BInst (Sub, RegOp RSP, RegOp RSP, ImmOp (ImmNum (-128L)));
							Pop (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							Jmp ".Ret";
						]
					};
					{
						label = ".LFE3";
						insts = [
						]
					};
					{
						label = ".Ret";
						insts = [
						]
					};
				]
				;
			func_type =
				[
					{
						label = "salsa20_block";
						pc = 5;
						reg_type = 
							[
								SingleVar (0);
								SingleVar (1);
								SingleVar (2);
								SingleVar (3);
								SingleVar (4);
								SingleVar (5);
								SingleVar (6);
								SingleVar (7);
								SingleVar (8);
								SingleVar (9);
								SingleVar (10);
								SingleVar (11);
								SingleVar (12);
								SingleVar (13);
								SingleVar (14);
								SingleVar (15);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (93));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (94));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (95));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleTop);
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleTop);
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L)))], SingleTop);
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFB3";
						pc = 7;
						reg_type = 
							[
								SingleVar (17);
								SingleVar (18);
								SingleVar (19);
								SingleVar (20);
								SingleVar (21);
								SingleVar (22);
								SingleVar (23);
								SingleVar (24);
								SingleVar (25);
								SingleVar (26);
								SingleVar (27);
								SingleVar (28);
								SingleVar (29);
								SingleVar (30);
								SingleVar (31);
								SingleVar (32);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (96));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (97));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (98));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (33));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (34));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L)))], SingleVar (35));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [21; 23];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L7";
						pc = 30;
						reg_type = 
							[
								SingleVar (36);
								SingleVar (37);
								SingleVar (38);
								SingleVar (39);
								SingleVar (40);
								SingleVar (41);
								SingleVar (42);
								SingleVar (43);
								SingleVar (44);
								SingleVar (45);
								SingleVar (46);
								SingleVar (47);
								SingleVar (48);
								SingleVar (49);
								SingleVar (50);
								SingleVar (51);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (99));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (100));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (101));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (52));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (53));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L)))], SingleVar (54));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [36; 39; 40];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFE3";
						pc = 46;
						reg_type = 
							[
								SingleVar (55);
								SingleVar (56);
								SingleVar (57);
								SingleVar (58);
								SingleVar (59);
								SingleVar (60);
								SingleVar (61);
								SingleVar (62);
								SingleVar (63);
								SingleVar (64);
								SingleVar (65);
								SingleVar (66);
								SingleVar (67);
								SingleVar (68);
								SingleVar (69);
								SingleVar (70);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (102));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (103));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (104));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (71));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (72));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L)))], SingleVar (73));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".Ret";
						pc = 47;
						reg_type = 
							[
								SingleVar (74);
								SingleVar (75);
								SingleVar (76);
								SingleVar (77);
								SingleVar (78);
								SingleVar (79);
								SingleVar (80);
								SingleVar (81);
								SingleVar (82);
								SingleVar (83);
								SingleVar (84);
								SingleVar (85);
								SingleVar (86);
								SingleVar (87);
								SingleVar (88);
								SingleVar (89);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (105));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (106));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (107));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (90));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), RangeConst [], SingleVar (91));
								]);
								(6, [
									((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L))), RangeConst [(SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (32L)))], SingleVar (92));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [77; 78; 79; 86; 87; 88; 89; 91; 92];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
				]
				;
			single_subtype =
				[
					{ var_idx = (15, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (14, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (13, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (12, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (5, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (3, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (35, 7); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (20, 7); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (32, 7); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (31, 7); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (30, 7); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (29, 7); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (22, 7); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (7, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (54, 30); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (92, 47); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (91, 47); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (51, 30); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (89, 47); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (50, 30); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (88, 47); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (49, 30); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (87, 47); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (48, 30); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (86, 47); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (41, 30); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (79, 47); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (78, 47); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
					{ var_idx = (101, 30); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (77, 47); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (24, 7); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (40, 30); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (39, 30); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (36, 30); sol = SolCond (41, Range (SingleConst (0L), SingleConst (63L), 1L), Range (SingleConst (0L), SingleConst (62L), 1L), Single (SingleConst (63L))); subtype_list = []; supertype_list = [] };
					{ var_idx = (6, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (23, 7); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (4, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (21, 7); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
				]
				;
			next_var = SingleTop;
			input_var_set = SingleExp.SingleVarSet.of_list [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
			smt_ctx = SmtEmitter.init_smt_ctx ();
		}
		;
		{
			func_name = "salsa20";
			func =
				[
					{
						label = "salsa20";
						insts = [
							Jmp ".LFB4";
						]
					};
					{
						label = ".LFB4";
						insts = [
							Test (RegOp RSI, RegOp RSI);
							Jcond (JE, ".L19");
							Push (RegOp R15, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							UInst (Mov, RegOp R15, RegOp RSI);
							Push (RegOp R14, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), true), None));
							UInst (Mov, RegOp R14, RegOp RDI);
							Push (RegOp R13, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), true), None));
							Push (RegOp R12, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), true), None));
							UInst (Mov, RegOp R12, RegOp RCX);
							Push (RegOp RBP, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), true), None));
							UInst (Mov, RegOp RBP, RegOp RDX);
							Push (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), true), None));
							BInst (Xor, RegOp EBX, RegOp EBX, RegOp EBX);
							BInst (Sub, RegOp RSP, RegOp RSP, ImmOp (ImmNum 72L));
							UInst (Mov, RegOp R13, RegOp RSP);
							Jmp ".L14";
						]
					};
					{
						label = ".L14";
						insts = [
							UInst (Mov, RegOp EAX, RegOp EBX);
							BInst (And, RegOp EAX, RegOp EAX, ImmOp (ImmNum 63L));
							Jcond (JE, ".L22");
							UInst (MovS, RegOp RAX, RegOp EAX);
							UInst (MovZ, RegOp EAX, LdOp(None, Some (RSP), Some (RAX), None, 1L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), false), None)));
							BInst (Xor, StOp(None, Some (R14), Some (RBX), None, 1L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), false), None)), LdOp(None, Some (R14), Some (RBX), None, 1L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), false), None)), RegOp AL);
							BInst (Add, RegOp RBX, RegOp RBX, ImmOp (ImmNum 1L));
							Cmp (RegOp R15, RegOp RBX);
							Jcond (JNe, ".L14");
							BInst (Add, RegOp RSP, RegOp RSP, ImmOp (ImmNum 72L));
							Pop (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), true), None));
							Pop (RegOp RBP, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), true), None));
							Pop (RegOp R12, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), true), None));
							Pop (RegOp R13, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), true), None));
							Pop (RegOp R14, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), true), None));
							Pop (RegOp R15, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							Jmp ".Ret";
						]
					};
					{
						label = ".L22";
						insts = [
							UInst (Mov, RegOp ECX, RegOp EBX);
							UInst (Mov, RegOp RDX, RegOp R12);
							UInst (Mov, RegOp RSI, RegOp RBP);
							UInst (Mov, RegOp RDI, RegOp R13);
							BInst (Sar, RegOp ECX, RegOp ECX, ImmOp (ImmNum 6L));
							UInst (MovS, RegOp RCX, RegOp ECX);
							Call "salsa20_block";
							UInst (MovZ, RegOp EAX, LdOp(None, Some (RSP), None, None, 1L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), false), None)));
							BInst (Xor, StOp(None, Some (R14), Some (RBX), None, 1L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), false), None)), LdOp(None, Some (R14), Some (RBX), None, 1L, (Some (7, (SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), false), None)), RegOp AL);
							BInst (Add, RegOp RBX, RegOp RBX, ImmOp (ImmNum 1L));
							Cmp (RegOp R15, RegOp RBX);
							Jcond (JNe, ".L14");
							BInst (Add, RegOp RSP, RegOp RSP, ImmOp (ImmNum 72L));
							Pop (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), true), None));
							Pop (RegOp RBP, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), true), None));
							Pop (RegOp R12, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), true), None));
							Pop (RegOp R13, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), true), None));
							Pop (RegOp R14, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), true), None));
							Pop (RegOp R15, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							Jmp ".Ret";
						]
					};
					{
						label = ".L19";
						insts = [
							Jmp ".Ret";
						]
					};
					{
						label = ".LFE4";
						insts = [
						]
					};
					{
						label = ".Ret";
						insts = [
						]
					};
				]
				;
			func_type =
				[
					{
						label = "salsa20";
						pc = 5;
						reg_type = 
							[
								SingleVar (0);
								SingleVar (1);
								SingleVar (2);
								SingleVar (3);
								SingleVar (4);
								SingleVar (5);
								SingleVar (6);
								SingleVar (7);
								SingleVar (8);
								SingleVar (9);
								SingleVar (10);
								SingleVar (11);
								SingleVar (12);
								SingleVar (13);
								SingleVar (14);
								SingleVar (15);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (131));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (132));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (133));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (134));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (135));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (136));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (137));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleTop);
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleTop);
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleTop);
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFB4";
						pc = 7;
						reg_type = 
							[
								SingleVar (17);
								SingleVar (18);
								SingleVar (19);
								SingleVar (20);
								SingleVar (21);
								SingleVar (22);
								SingleVar (23);
								SingleVar (24);
								SingleVar (25);
								SingleVar (26);
								SingleVar (27);
								SingleVar (28);
								SingleVar (29);
								SingleVar (30);
								SingleVar (31);
								SingleVar (32);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (138));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (139));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (140));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (141));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (142));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (143));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (144));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (33));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (34));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (35));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [21; 23];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L14";
						pc = 24;
						reg_type = 
							[
								SingleVar (36);
								SingleVar (37);
								SingleVar (38);
								SingleVar (39);
								SingleVar (40);
								SingleVar (41);
								SingleVar (42);
								SingleVar (43);
								SingleVar (44);
								SingleVar (45);
								SingleVar (46);
								SingleVar (47);
								SingleVar (48);
								SingleVar (49);
								SingleVar (50);
								SingleVar (51);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (145));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (146));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (147));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (148));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (149));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (150));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (151));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (52));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (53));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (54));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [39; 40; 50; 51];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L22";
						pc = 42;
						reg_type = 
							[
								SingleVar (55);
								SingleVar (56);
								SingleVar (57);
								SingleVar (58);
								SingleVar (59);
								SingleVar (60);
								SingleVar (61);
								SingleVar (62);
								SingleVar (63);
								SingleVar (64);
								SingleVar (65);
								SingleVar (66);
								SingleVar (67);
								SingleVar (68);
								SingleVar (69);
								SingleVar (70);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (152));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (153));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (154));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (155));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (156));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (157));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (158));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (71));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (72));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (73));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [58; 59; 60; 68; 69; 70];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L19";
						pc = 63;
						reg_type = 
							[
								SingleVar (74);
								SingleVar (75);
								SingleVar (76);
								SingleVar (77);
								SingleVar (78);
								SingleVar (79);
								SingleVar (80);
								SingleVar (81);
								SingleVar (82);
								SingleVar (83);
								SingleVar (84);
								SingleVar (85);
								SingleVar (86);
								SingleVar (87);
								SingleVar (88);
								SingleVar (89);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (159));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (160));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (161));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (162));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (163));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (164));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (165));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (90));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (91));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (92));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFE4";
						pc = 65;
						reg_type = 
							[
								SingleVar (93);
								SingleVar (94);
								SingleVar (95);
								SingleVar (96);
								SingleVar (97);
								SingleVar (98);
								SingleVar (99);
								SingleVar (100);
								SingleVar (101);
								SingleVar (102);
								SingleVar (103);
								SingleVar (104);
								SingleVar (105);
								SingleVar (106);
								SingleVar (107);
								SingleVar (108);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (166));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (167));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (168));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (169));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (170));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (171));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (172));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (109));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (110));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (111));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".Ret";
						pc = 66;
						reg_type = 
							[
								SingleVar (112);
								SingleVar (113);
								SingleVar (114);
								SingleVar (115);
								SingleVar (116);
								SingleVar (117);
								SingleVar (118);
								SingleVar (119);
								SingleVar (120);
								SingleVar (121);
								SingleVar (122);
								SingleVar (123);
								SingleVar (124);
								SingleVar (125);
								SingleVar (126);
								SingleVar (127);
							]
						;
						mem_type = 
							[
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), RangeConst [], SingleVar (173));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), RangeConst [], SingleVar (174));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), RangeConst [], SingleVar (175));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), RangeConst [], SingleVar (176));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), RangeConst [], SingleVar (177));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (178));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (179));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (128));
								]);
								(7, [
									((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6))), RangeConst [(SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleVar (6)))], SingleVar (129));
								]);
								(2, [
									((SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L))), RangeConst [(SingleVar (2), SingleBExp (SingleAdd, SingleVar (2), SingleConst (32L)))], SingleVar (130));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [115; 116; 117; 124; 125; 126; 127; 129; 130];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
				]
				;
			single_subtype =
				[
					{ var_idx = (15, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (14, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (13, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (12, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (5, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (3, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (2, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (34, 7); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (35, 7); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (32, 7); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (31, 7); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (30, 7); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (29, 7); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (22, 7); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (20, 7); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (19, 7); sol = SolSimple (Single (SingleVar (2))); subtype_list = []; supertype_list = [] };
					{ var_idx = (7, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (54, 24); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (151, 24); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (150, 24); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (149, 24); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (148, 24); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (147, 24); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (146, 24); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (158, 42); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (157, 42); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (156, 42); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (155, 42); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (154, 42); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (153, 42); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (92, 63); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (130, 66); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (91, 63); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (129, 66); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (89, 63); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (127, 66); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (88, 63); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (126, 66); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (87, 63); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (125, 66); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (86, 63); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (124, 66); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (79, 63); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (117, 66); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (78, 63); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
					{ var_idx = (116, 66); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
					{ var_idx = (77, 63); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (115, 66); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (49, 24); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (68, 42); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (41, 24); sol = SolSimple (Single (SingleVar (2))); subtype_list = []; supertype_list = [] };
					{ var_idx = (60, 42); sol = SolSimple (Single (SingleVar (2))); subtype_list = []; supertype_list = [] };
					{ var_idx = (58, 42); sol = SolCond (53, Range (SingleConst (0L), SingleBExp (SingleAdd, SingleVar (6), SingleConst (-1L)), 1L), Range (SingleConst (0L), SingleBExp (SingleAdd, SingleVar (6), SingleConst (-2L)), 1L), Single (SingleBExp (SingleAdd, SingleVar (6), SingleConst (-1L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (24, 7); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (70, 42); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (51, 24); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (69, 42); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (50, 24); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
					{ var_idx = (59, 42); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (40, 24); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (39, 24); sol = SolCond (32, Range (SingleConst (0L), SingleBExp (SingleAdd, SingleVar (6), SingleConst (-1L)), 1L), Range (SingleConst (0L), SingleBExp (SingleAdd, SingleVar (6), SingleConst (-2L)), 1L), Single (SingleBExp (SingleAdd, SingleVar (6), SingleConst (-1L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (6, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (23, 7); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
					{ var_idx = (4, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (21, 7); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
				]
				;
			next_var = SingleTop;
			input_var_set = SingleExp.SingleVarSet.of_list [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
			smt_ctx = SmtEmitter.init_smt_ctx ();
		}
		;
		{
			func_name = "_start";
			func =
				[
					{
						label = "_start";
						insts = [
							Jmp ".LFB5";
						]
					};
					{
						label = ".LFB5";
						insts = [
							Push (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							BInst (Xor, RegOp EDX, RegOp EDX, RegOp EDX);
							BInst (Sub, RegOp RSP, RegOp RSP, ImmOp (ImmNum 240L));
							UInst (Mov, StOp(Some (ImmNum 32L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), true), None)), ImmOp (ImmNum 1296236545L));
							UInst (Lea, RegOp RAX, MemOp (Some (ImmNum 32L), Some (RSP), None, None));
							UInst (Mov, StOp(Some (ImmNum 40L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), true), None)), ImmOp (ImmLabel (-2)));
							UInst (Mov, StOp(Some (ImmNum 48L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), true), None)), ImmOp (ImmNum 32L));
							UInst (Mov, StOp(Some (ImmNum 56L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 64L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 72L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), true), None)), ImmOp (ImmNum 0L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 3L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 13L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 61L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 51L));
							Xchg (RegOp RBX, RegOp RBX, RegOp RBX, RegOp RBX);
							UInst (Mov, StOp(Some (ImmNum 8L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), true), None)), RegOp RDX);
							BInst (Xor, RegOp EDX, RegOp EDX, RegOp EDX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 8L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), true), None)));
							UInst (Lea, RegOp RAX, MemOp (Some (ImmNum 80L), Some (RSP), None, None));
							UInst (Mov, StOp(Some (ImmNum 80L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), true), None)), ImmOp (ImmNum 1296236545L));
							UInst (Mov, StOp(Some (ImmNum 88L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), true), None)), ImmOp (ImmLabel (-3)));
							UInst (Mov, StOp(Some (ImmNum 96L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), true), None)), ImmOp (ImmNum 8L));
							UInst (Mov, StOp(Some (ImmNum 104L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 112L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 120L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), true), None)), ImmOp (ImmNum 0L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 3L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 13L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 61L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 51L));
							Xchg (RegOp RBX, RegOp RBX, RegOp RBX, RegOp RBX);
							UInst (Mov, StOp(Some (ImmNum 16L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), true), None)), RegOp RDX);
							BInst (Xor, RegOp EDX, RegOp EDX, RegOp EDX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 16L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), true), None)));
							UInst (Lea, RegOp RAX, MemOp (Some (ImmNum 128L), Some (RSP), None, None));
							UInst (Mov, StOp(Some (ImmNum 128L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), true), None)), ImmOp (ImmNum 1296236545L));
							UInst (Mov, StOp(Some (ImmNum 136L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), true), None)), ImmOp (ImmLabel (-4)));
							UInst (Mov, StOp(Some (ImmNum 144L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), true), None)), ImmOp (ImmNum 64L));
							UInst (Mov, StOp(Some (ImmNum 152L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 160L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), true), None)), ImmOp (ImmNum 0L));
							UInst (Mov, StOp(Some (ImmNum 168L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), true), None)), ImmOp (ImmNum 0L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 3L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 13L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 61L));
							BInst (Rol, RegOp RDI, RegOp RDI, ImmOp (ImmNum 51L));
							Xchg (RegOp RBX, RegOp RBX, RegOp RBX, RegOp RBX);
							UInst (Mov, StOp(Some (ImmNum 24L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), true), None)), RegOp RDX);
							UInst (Lea, RegOp RBX, MemOp (Some (ImmNum 176L), Some (RSP), None, None));
							BInst (Xor, RegOp ECX, RegOp ECX, RegOp ECX);
							UInst (Mov, RegOp RDX, LdOp(Some (ImmLabel (-3)), None, None, None, 8L, (Some (-3, (SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), true), None)));
							UInst (Mov, RegOp ESI, ImmOp (ImmLabel (-2)));
							UInst (Mov, RegOp RDI, RegOp RBX);
							UInst (Mov, RegOp RAX, LdOp(Some (ImmNum 24L), Some (RSP), None, None, 8L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), true), None)));
							Call "salsa20_block";
							UInst (MovZ, RegOp EAX, LdOp(Some (ImmLabel (-4)), None, None, None, 1L, (Some (-4, (SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), false), None)));
							BInst (Xor, RegOp AL, RegOp AL, LdOp(Some (ImmNum 176L), Some (RSP), None, None, 1L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), false), None)));
							UInst (Mov, StOp(Some (ImmLabel (-4)), None, None, None, 1L, (Some (-4, (SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), false), None)), RegOp AL);
							UInst (Mov, RegOp EAX, ImmOp (ImmNum 1L));
							Jmp ".L24";
						]
					};
					{
						label = ".L24";
						insts = [
							UInst (MovZ, RegOp EDX, LdOp(Some (ImmLabel (-4)), Some (RAX), None, None, 1L, (Some (-4, (SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), false), None)));
							BInst (Xor, RegOp DL, RegOp DL, LdOp(None, Some (RBX), Some (RAX), None, 1L, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), false), None)));
							BInst (Add, RegOp RAX, RegOp RAX, ImmOp (ImmNum 1L));
							UInst (Mov, StOp(Some (ImmBExp (ImmLabel (-4), ImmNum (-1L))), Some (RAX), None, None, 1L, (Some (-4, (SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), false), None)), RegOp DL);
							Cmp (RegOp RAX, ImmOp (ImmNum 64L));
							Jcond (JNe, ".L24");
							UInst (Mov, RegOp EAX, ImmOp (ImmNum 60L));
							BInst (Xor, RegOp EDI, RegOp EDI, RegOp EDI);
							Syscall;
							Hlt;
							BInst (Add, RegOp RSP, RegOp RSP, ImmOp (ImmNum 240L));
							Pop (RegOp RBX, (Some (4, (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), true), None));
							Jmp ".Ret";
						]
					};
					{
						label = ".LFE5";
						insts = [
						]
					};
					{
						label = ".Ret";
						insts = [
						]
					};
				]
				;
			func_type =
				[
					{
						label = "_start";
						pc = 5;
						reg_type = 
							[
								SingleVar (0);
								SingleVar (1);
								SingleVar (2);
								SingleVar (3);
								SingleVar (4);
								SingleVar (5);
								SingleVar (6);
								SingleVar (7);
								SingleVar (8);
								SingleVar (9);
								SingleVar (10);
								SingleVar (11);
								SingleVar (12);
								SingleVar (13);
								SingleVar (14);
								SingleVar (15);
							]
						;
						mem_type = 
							[
								(-2, [
									((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L))), RangeConst [(SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L)))], SingleTop);
								]);
								(-3, [
									((SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), RangeConst [(SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L)))], SingleTop);
								]);
								(-4, [
									((SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), RangeConst [(SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L)))], SingleTop);
								]);
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), RangeConst [], SingleVar (97));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), RangeConst [], SingleVar (98));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), RangeConst [], SingleVar (99));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), RangeConst [], SingleVar (100));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), RangeConst [], SingleVar (101));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), RangeConst [], SingleVar (102));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), RangeConst [], SingleVar (103));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), RangeConst [], SingleVar (104));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), RangeConst [], SingleVar (105));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), RangeConst [], SingleVar (106));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), RangeConst [], SingleVar (107));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), RangeConst [], SingleVar (108));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), RangeConst [], SingleVar (109));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), RangeConst [], SingleVar (110));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), RangeConst [], SingleVar (111));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), RangeConst [], SingleVar (112));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), RangeConst [], SingleVar (113));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (114));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (115));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (116));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (117));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (118));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (119));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleTop);
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFB5";
						pc = 7;
						reg_type = 
							[
								SingleVar (17);
								SingleVar (18);
								SingleVar (19);
								SingleVar (20);
								SingleVar (21);
								SingleVar (22);
								SingleVar (23);
								SingleVar (24);
								SingleVar (25);
								SingleVar (26);
								SingleVar (27);
								SingleVar (28);
								SingleVar (29);
								SingleVar (30);
								SingleVar (31);
								SingleVar (32);
							]
						;
						mem_type = 
							[
								(-2, [
									((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L))), RangeConst [(SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L)))], SingleVar (33));
								]);
								(-3, [
									((SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), RangeConst [(SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L)))], SingleVar (34));
								]);
								(-4, [
									((SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), RangeConst [(SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L)))], SingleVar (35));
								]);
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), RangeConst [], SingleVar (120));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), RangeConst [], SingleVar (121));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), RangeConst [], SingleVar (122));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), RangeConst [], SingleVar (123));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), RangeConst [], SingleVar (124));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), RangeConst [], SingleVar (125));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), RangeConst [], SingleVar (126));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), RangeConst [], SingleVar (127));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), RangeConst [], SingleVar (128));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), RangeConst [], SingleVar (129));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), RangeConst [], SingleVar (130));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), RangeConst [], SingleVar (131));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), RangeConst [], SingleVar (132));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), RangeConst [], SingleVar (133));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), RangeConst [], SingleVar (134));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), RangeConst [], SingleVar (135));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), RangeConst [], SingleVar (136));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (137));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (138));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (139));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (140));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (141));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (142));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (36));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [-4; -3; -2; 21];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".L24";
						pc = 66;
						reg_type = 
							[
								SingleVar (37);
								SingleVar (38);
								SingleVar (39);
								SingleVar (40);
								SingleVar (41);
								SingleVar (42);
								SingleVar (43);
								SingleVar (44);
								SingleVar (45);
								SingleVar (46);
								SingleVar (47);
								SingleVar (48);
								SingleVar (49);
								SingleVar (50);
								SingleVar (51);
								SingleVar (52);
							]
						;
						mem_type = 
							[
								(-2, [
									((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L))), RangeConst [(SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L)))], SingleVar (53));
								]);
								(-3, [
									((SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), RangeConst [(SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L)))], SingleVar (54));
								]);
								(-4, [
									((SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), RangeConst [(SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L)))], SingleVar (55));
								]);
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), RangeConst [], SingleVar (143));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), RangeConst [], SingleVar (144));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), RangeConst [], SingleVar (145));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), RangeConst [], SingleVar (146));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), RangeConst [], SingleVar (147));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), RangeConst [], SingleVar (148));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), RangeConst [], SingleVar (149));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), RangeConst [], SingleVar (150));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), RangeConst [], SingleVar (151));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), RangeConst [], SingleVar (152));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), RangeConst [], SingleVar (153));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), RangeConst [], SingleVar (154));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), RangeConst [], SingleVar (155));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), RangeConst [], SingleVar (156));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), RangeConst [], SingleVar (157));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), RangeConst [], SingleVar (158));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), RangeConst [], SingleVar (159));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (160));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (161));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (162));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (163));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (164));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (165));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (56));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [-4; 37; 40; 41];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".LFE5";
						pc = 80;
						reg_type = 
							[
								SingleVar (57);
								SingleVar (58);
								SingleVar (59);
								SingleVar (60);
								SingleVar (61);
								SingleVar (62);
								SingleVar (63);
								SingleVar (64);
								SingleVar (65);
								SingleVar (66);
								SingleVar (67);
								SingleVar (68);
								SingleVar (69);
								SingleVar (70);
								SingleVar (71);
								SingleVar (72);
							]
						;
						mem_type = 
							[
								(-2, [
									((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L))), RangeConst [(SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L)))], SingleVar (73));
								]);
								(-3, [
									((SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), RangeConst [(SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L)))], SingleVar (74));
								]);
								(-4, [
									((SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), RangeConst [(SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L)))], SingleVar (75));
								]);
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), RangeConst [], SingleVar (166));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), RangeConst [], SingleVar (167));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), RangeConst [], SingleVar (168));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), RangeConst [], SingleVar (169));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), RangeConst [], SingleVar (170));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), RangeConst [], SingleVar (171));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), RangeConst [], SingleVar (172));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), RangeConst [], SingleVar (173));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), RangeConst [], SingleVar (174));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), RangeConst [], SingleVar (175));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), RangeConst [], SingleVar (176));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), RangeConst [], SingleVar (177));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), RangeConst [], SingleVar (178));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), RangeConst [], SingleVar (179));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), RangeConst [], SingleVar (180));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), RangeConst [], SingleVar (181));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), RangeConst [], SingleVar (182));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (183));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (184));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (185));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (186));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (187));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (188));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (76));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
					{
						label = ".Ret";
						pc = 81;
						reg_type = 
							[
								SingleVar (77);
								SingleVar (78);
								SingleVar (79);
								SingleVar (80);
								SingleVar (81);
								SingleVar (82);
								SingleVar (83);
								SingleVar (84);
								SingleVar (85);
								SingleVar (86);
								SingleVar (87);
								SingleVar (88);
								SingleVar (89);
								SingleVar (90);
								SingleVar (91);
								SingleVar (92);
							]
						;
						mem_type = 
							[
								(-2, [
									((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L))), RangeConst [(SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (32L)))], SingleVar (93));
								]);
								(-3, [
									((SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L))), RangeConst [(SingleVar (-3), SingleBExp (SingleAdd, SingleVar (-3), SingleConst (8L)))], SingleVar (94));
								]);
								(-4, [
									((SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L))), RangeConst [(SingleVar (-4), SingleBExp (SingleAdd, SingleVar (-4), SingleConst (64L)))], SingleVar (95));
								]);
								(4, [
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), RangeConst [], SingleVar (189));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), RangeConst [], SingleVar (190));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), RangeConst [], SingleVar (191));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), RangeConst [], SingleVar (192));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), RangeConst [], SingleVar (193));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), RangeConst [], SingleVar (194));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), RangeConst [], SingleVar (195));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), RangeConst [], SingleVar (196));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), RangeConst [], SingleVar (197));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), RangeConst [], SingleVar (198));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), RangeConst [], SingleVar (199));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), RangeConst [], SingleVar (200));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), RangeConst [], SingleVar (201));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), RangeConst [], SingleVar (202));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), RangeConst [], SingleVar (203));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), RangeConst [], SingleVar (204));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), RangeConst [], SingleVar (205));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), RangeConst [], SingleVar (206));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), RangeConst [], SingleVar (207));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), RangeConst [], SingleVar (208));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), RangeConst [], SingleVar (209));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), RangeConst [], SingleVar (210));
									((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), RangeConst [], SingleVar (211));
									((SingleVar (4), SingleVar (4)), RangeConst [], SingleVar (96));
								]);
							]
						;
						flag = (SingleTop, SingleTop);
						branch_hist = [];
						full_not_taken_hist = [];
						constraint_list = [];
						local_var_map = [];
						useful_var = SingleExp.SingleVarSet.of_list [80; 81; 82; 89; 90; 91; 92; 93; 94; 95];
						global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
						prop_mode = TypeInferDep;
					}
					;
				]
				;
			single_subtype =
				[
					{ var_idx = (15, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (14, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (13, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (12, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (5, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (3, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (20, 7); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (34, 7); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (32, 7); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (31, 7); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (30, 7); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (29, 7); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (22, 7); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (95, 81); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (54, 66); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (94, 81); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (53, 66); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (93, 81); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
					{ var_idx = (52, 66); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (92, 81); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
					{ var_idx = (51, 66); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (91, 81); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
					{ var_idx = (50, 66); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (90, 81); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
					{ var_idx = (49, 66); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (89, 81); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
					{ var_idx = (42, 66); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (82, 81); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
					{ var_idx = (81, 81); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
					{ var_idx = (165, 66); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (80, 81); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
					{ var_idx = (41, 66); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-248L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (40, 66); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)))); subtype_list = []; supertype_list = [] };
					{ var_idx = (37, 66); sol = SolCond (71, Range (SingleConst (1L), SingleConst (63L), 1L), Range (SingleConst (1L), SingleConst (62L), 1L), Single (SingleConst (63L))); subtype_list = []; supertype_list = [] };
					{ var_idx = (4, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
					{ var_idx = (21, 7); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
				]
				;
			next_var = SingleTop;
			input_var_set = SingleExp.SingleVarSet.of_list [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
			smt_ctx = SmtEmitter.init_smt_ctx ();
		}
		;
	]
	
	