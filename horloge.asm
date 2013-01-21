 
	(* contenu de la mémoire:
		0: 0 au départ : 0 si calcul pas fini, 1 si calcul fini 
		1: secondes
		2: minutes
		3: heures
		4: jour
	*)
	li 1
	move $cx, $ax
	li 0
	move $dx, $ax
secondes:
	li 1
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 60
	move $bx,$ax
	test $ex,$bx
	jme minutes
	sw $sp, $ex
	jmp exit
minutes:
	sw $sp,$dx 	
	li 2
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 60
	move $bx,$ax
	test $ex,$bx
	jme heures
	sw $sp, $ex
	jmp exit
heures:
	sw $sp,$dx
	li 3
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 24
	move $bx,$ax
	test $ex,$bx
	jme jour
	sw $sp, $ex
	jmp exit
jour:
	sw $sp,$dx	
	li 4
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	sw $sp,$ex
exit:
	li 0
	move $sp,$ax
	sw $sp,$cx
	jmp secondes
	  
