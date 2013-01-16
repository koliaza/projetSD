div:  (*entrée: ax, bx. Sortie: cx = ax/bx*)
		
	li $cx,0
        li $dx,1
        jmp div_2
div_1:
	add cx,dx
div_2: 
	move ex,cx	
	mul $ex,$bx
	test $ex,$ax
	jle div_1
	
	
	sub $cx,$dx
	jr $ra

mod: (*entrée: ax,bx. Sortie: cx = ax mod bx *)
	jal div
        mul $cx,$bx
	move $dx,$cx
	move $cx,$ax
	sub $cx,$dx 
	jr $ra
