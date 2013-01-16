
	(* contenu de la mémoire:
		0: 0 au départ : 0 si calcul pas fini, 1 si calcul fini 
		1: secondes
		2: minutes
		3: heures
		4: jour
	*)
	li cx,1
secondes:
	li 1
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 60
	move $bx,ax
	test $ex,$bx
	jm minutes
	sw $sp, $ex
	jmp exit
minutes:
	sw $sp,$cx 	
	li 2
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 60
	move $bx,ax
	test $ex,$bx
	jm heures
	sw $sp, $ex
	jmp exit
heures:
	sw $sp,$cx
	li 3
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	li 24
	move $bx,ax
	test $ex,$bx
	jm jour
	sw $sp, $ex
	jmp exit
jour:
	sw $sp,$cx	
	li 4
        move $sp, $ax
        lw $ex, $sp
	add $ex,$cx
	sw $sp,$ex
exit:
	li $sp,0
	sw $sp,$cx
	jmp secondes
	

	  
