# start of generated code
	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
_int_tag:
	.word	2
_bool_tag:
	.word	3
_string_tag:
	.word	4
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
str_const15:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const1
	.byte	0	
	.align	2
	.word	-1
str_const14:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const13:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"Cons"
	.byte	0	
	.align	2
	.word	-1
str_const12:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"List"
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const6
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const3
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const2
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const6
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const7
	.ascii	"_prim_slot"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const8
	.ascii	"SELF_TYPE"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const8
	.ascii	"_no_class"
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	4
	.word	8
	.word	String_dispTab
	.word	int_const9
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.ascii	" "
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.ascii	"\n"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const7
	.ascii	"example.cl"
	.byte	0	
	.align	2
	.word	-1
int_const9:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const8:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	9
	.word	-1
int_const7:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	10
	.word	-1
int_const6:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const5:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	5
	.word	-1
int_const4:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const3:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const2:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const1:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
int_const0:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	1
	.word	-1
bool_const0:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:
	.word	str_const7
# child: IO
# child: Int
# child: Bool
# child: String
# child: List

	.word	str_const8
# child: Main

	.word	str_const9

	.word	str_const10

	.word	str_const11

	.word	str_const12
# child: Cons

	.word	str_const13

	.word	str_const14

class_objTab:
	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
	.word	List_protObj
	.word	List_init
	.word	Cons_protObj
	.word	Cons_init
	.word	Main_protObj
	.word	Main_init
Object_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
IO_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
	# method # 3
	.word	IO.out_string
	# method # 4
	.word	IO.out_int
	# method # 5
	.word	IO.in_string
	# method # 6
	.word	IO.in_int
Int_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
Bool_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
String_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
	# method # 3
	.word	String.length
	# method # 4
	.word	String.concat
	# method # 5
	.word	String.substr
List_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
	# method # 3
	.word	List.isNil
	# method # 4
	.word	List.head
	# method # 5
	.word	List.tail
	# method # 6
	.word	List.cons
Cons_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
	# method # 3
	.word	Cons.isNil
	# method # 4
	.word	Cons.head
	# method # 5
	.word	Cons.tail
	# method # 6
	.word	List.cons
	# method # 7
	.word	Cons.init
Main_dispTab:
	# method # 0
	.word	Object.abort
	# method # 1
	.word	Object.type_name
	# method # 2
	.word	Object.copy
	# method # 3
	.word	IO.out_string
	# method # 4
	.word	IO.out_int
	# method # 5
	.word	IO.in_string
	# method # 6
	.word	IO.in_int
	# method # 7
	.word	Main.print_list
	# method # 8
	.word	Main.main
	.word	-1
Object_protObj:
	.word	0	# class tag
	.word	3	# size
	.word	Object_dispTab
	.word	-1
IO_protObj:
	.word	1	# class tag
	.word	3	# size
	.word	IO_dispTab
	.word	-1
Int_protObj:
	.word	2	# class tag
	.word	4	# size
	.word	Int_dispTab
	.word	0	# val(0)
	.word	-1
Bool_protObj:
	.word	3	# class tag
	.word	4	# size
	.word	Bool_dispTab
	.word	0	# val(0)
	.word	-1
String_protObj:
	.word	4	# class tag
	.word	5	# size
	.word	String_dispTab
	.word	int_const1	# int(0)
	.word	0	# str(0)
	.word	-1
List_protObj:
	.word	5	# class tag
	.word	3	# size
	.word	List_dispTab
	.word	-1
Cons_protObj:
	.word	6	# class tag
	.word	5	# size
	.word	Cons_dispTab
	.word	int_const1	# int(0)
	.word	0	# void
	.word	-1
Main_protObj:
	.word	7	# class tag
	.word	4	# size
	.word	Main_dispTab
	.word	0	# void
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
Object_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

IO_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	Object_init

	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

Int_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	Object_init

	# init attrib _val
	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

Bool_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	Object_init

	# init attrib _val
	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

String_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	Object_init

	# init attrib _val
	la	$a0 int_const1
	sw	$a0 12($s0)
	# init attrib _str_field
	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

List_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	Object_init

	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

Cons_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	List_init

	# init attrib car
	la	$a0 int_const1
	sw	$a0 12($s0)
	# init attrib cdr
	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

Main_init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# init parent
	jal	IO_init

	# init attrib mylist
	# ret = SELF
	move	$a0 $s0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# return
	jr	$ra	

List.isNil:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	la	$a0 bool_const1

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

List.head:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label0
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label0:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 0($t1)

	# jumpto abort
	jalr		$t1

	la	$a0 int_const1

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

List.tail:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label1
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label1:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 0($t1)

	# jumpto abort
	jalr		$t1

	# Object:
	# self
	move	$a0 $s0


	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

List.cons:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Dispatch. First eval and save the params.
	# Object:
	# param
	lw	$a0 12($fp)

	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# Object:
	# self
	move	$a0 $s0

	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	la	$a0 Cons_protObj
	jal	Object.copy
	jal	Cons_init
	# if obj = void: abort
	bne	$a0 $zero label2
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label2:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 28($t1)

	# jumpto init
	jalr		$t1


	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 4

	# return
	jr	$ra	

Cons.isNil:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	la	$a0 bool_const0

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

Cons.head:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Object:
	# attribute
	lw	$a0 12($s0)


	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

Cons.tail:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Object:
	# attribute
	lw	$a0 16($s0)


	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	

Cons.init:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Assign. First eval the expr.
	# Object:
	# param
	lw	$a0 16($fp)

	# Now find the lvalue.
	# It is an attribute.
	sw	$a0 12($s0)
	# Assign. First eval the expr.
	# Object:
	# param
	lw	$a0 12($fp)

	# Now find the lvalue.
	# It is an attribute.
	sw	$a0 16($s0)
	# Object:
	# self
	move	$a0 $s0


	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 8

	# return
	jr	$ra	

Main.print_list:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# If statement. First eval condition.
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# param
	lw	$a0 12($fp)

	# if obj = void: abort
	bne	$a0 $zero label3
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label3:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 12($t1)

	# jumpto isNil
	jalr		$t1

	# extract bool content from acc to t1
	lw	$t1 12($a0)

	# if t1 == 0 goto false
	beq	$t1 $zero label4

	# Dispatch. First eval and save the params.
	la	$a0 str_const1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label6
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label6:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 12($t1)

	# jumpto out_string
	jalr		$t1

	# jumpt finish
	b	label5

# False:
label4:
	# Dispatch. First eval and save the params.
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# param
	lw	$a0 12($fp)

	# if obj = void: abort
	bne	$a0 $zero label7
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label7:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 16($t1)

	# jumpto head
	jalr		$t1

	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label8
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label8:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 16($t1)

	# jumpto out_int
	jalr		$t1

	# Dispatch. First eval and save the params.
	la	$a0 str_const2
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label9
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label9:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 12($t1)

	# jumpto out_string
	jalr		$t1

	# Dispatch. First eval and save the params.
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# param
	lw	$a0 12($fp)

	# if obj = void: abort
	bne	$a0 $zero label10
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label10:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 20($t1)

	# jumpto tail
	jalr		$t1

	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label11
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label11:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 28($t1)

	# jumpto print_list
	jalr		$t1

# Finish:
label5:

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 4

	# return
	jr	$ra	

Main.main:
	# push fp, s0, ra
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)

	# fp now points to the return addr in the stack
	addiu	$fp $sp 4

	# SELF = a0
	move	$s0 $a0

	# evaluating expresion and put it in acc register
	# Assign. First eval the expr.
	# Dispatch. First eval and save the params.
	la	$a0 int_const5
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Dispatch. First eval and save the params.
	la	$a0 int_const4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Dispatch. First eval and save the params.
	la	$a0 int_const3
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Dispatch. First eval and save the params.
	la	$a0 int_const2
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Dispatch. First eval and save the params.
	la	$a0 int_const0
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	la	$a0 List_protObj
	jal	Object.copy
	jal	List_init
	# if obj = void: abort
	bne	$a0 $zero label12
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label12:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 24($t1)

	# jumpto cons
	jalr		$t1

	# if obj = void: abort
	bne	$a0 $zero label13
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label13:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 24($t1)

	# jumpto cons
	jalr		$t1

	# if obj = void: abort
	bne	$a0 $zero label14
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label14:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 24($t1)

	# jumpto cons
	jalr		$t1

	# if obj = void: abort
	bne	$a0 $zero label15
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label15:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 24($t1)

	# jumpto cons
	jalr		$t1

	# if obj = void: abort
	bne	$a0 $zero label16
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label16:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 24($t1)

	# jumpto cons
	jalr		$t1

	# Now find the lvalue.
	# It is an attribute.
	sw	$a0 12($s0)
	# While loop
	# start:
label17:
	# ACC = pred
	# the 'not' operator
	# First eval the bool
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# attribute
	lw	$a0 12($s0)

	# if obj = void: abort
	bne	$a0 $zero label19
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label19:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 12($t1)

	# jumpto isNil
	jalr		$t1

	# Extract the int inside the bool
	lw	$t1 12($a0)
	# Pretend ACC = false, then we need to construct true
	la	$a0 bool_const1
	# If ACC = false, jumpto finish
	beq	$t1 $zero label20
	# Load false
	la	$a0 bool_const0
	# finish:
label20:
	# extract int inside bool
	lw	$t1 12($a0)

	# if pred == false jumpto finish
	beq	$t1 $zero label18

	# Dispatch. First eval and save the params.
	# Object:
	# attribute
	lw	$a0 12($s0)

	sw	$a0 0($sp)
	addiu	$sp $sp -4
	# eval the obj in dispatch.
	# Object:
	# self
	move	$a0 $s0

	# if obj = void: abort
	bne	$a0 $zero label21
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label21:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 28($t1)

	# jumpto print_list
	jalr		$t1

	# Assign. First eval the expr.
	# Dispatch. First eval and save the params.
	# eval the obj in dispatch.
	# Object:
	# attribute
	lw	$a0 12($s0)

	# if obj = void: abort
	bne	$a0 $zero label22
	la	$a0 str_const0
	li	$t1 1
	jal	_dispatch_abort
label22:
	# locate method in dispatch table
	# t1 = self.dispTab
	lw	$t1 8($a0)

	# t1 = dispTab[offset]
	lw	$t1 20($t1)

	# jumpto tail
	jalr		$t1

	# Now find the lvalue.
	# It is an attribute.
	sw	$a0 12($s0)
	# Jumpto start
	b	label17
	# Finish:
label18:
	# ACC = void
	move	$a0 $zero

	# pop fp, s0, ra
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12

	# Pop arguments
	addiu	$sp $sp 0

	# return
	jr	$ra	


# end of generated code
