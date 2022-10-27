################################################################################
# eliminate - Triangularize matrix.
#
# Args:   	 $a0  - base address of matrix (A)
#   		 $a1  - number of elements per row (N)

eliminate:
   	 # If necessary, create stack frame, and save return address from ra
   	 addiu    $sp, $sp, -4   	 # allocate stack frame
   	 sw   	 $ra, 0($sp)   		 # done saving registers
   	 
   	 ##
   	 ## Implement eliminate here
   	 ## s0:     k
   	 ## s1:  i
   	 ## s2:     j
   	 ## s3:     rowoffset
   	 ## t0:    address to current pivot
   	 
   	 ## in div
   	 ## f0:     value of pivot
   	 ## t1: address to element j in row k

   	 ## in sub
   	 ## t1 address of [i][k]
   	 ## t2 address of current element
   	 ## t3 address of [k][j]
   	 ## f0 value of pivot column element of row
   	 ## f1 current elem
   	 ## f3 value of [k][j] then f0*f3

   	 addiu    $sp, $sp, -16   	 # allocate stack frame
   	 sw   	 $s3, 12($sp)
   	 sw   	 $s2, 8($sp)
   	 sw   	 $s1, 4($sp)
   	 sw   	 $s0, 0($sp)   		 # done saving registers

   	 move    $s0, $0
   	 move    $t0, $a0   		 # first pivot address
   	 sll   	 $s3, $a1, 2
   	 lwc1   	 $f10, fp0		# floating point 0
   	 lwc1     $f11, fp1		# floating point 1 

piv_row:

   	 lwc1     $f1, 0($t0)
   	 addiu    $s2, $s0, 1 # j = k+1
   	 addiu    $s1, $s0, 1 # i = k+1
   	 beq     $s2, $a1, end_div
   	 swc1    $f11, 0($t0)
   	 move    $t1, $t0
div:    
   	 addiu    $t1, $t1, 4   	 #address to elem j
   	 lwc1    $f2, 0($t1)
   	 addiu    $s2, $s2, 1   		 #inc j
   	 div.s     $f2, $f2, $f1
   	 bne    $s2, $a1, div    #if not end of row
   	 swc1    $f2, 0($t1)
   	 
    addu $t1, $t0, $zero
    addiu $s1, $s0, 1
    

sub_row:

    addiu    $s2, $s0, 1   	 # j = k+1
    addu   	 $t1, $t1, $s3    
    addu   	 $t3, $t0, $zero    
    addiu    $t2, $t1, 4   	 # to first elem after pivot column of row
    lwc1    $f0, 0($t1)
    swc1    $f10, 0($t1)   	 
    
sub:
   	 addiu    $t3, $t3, 4     # to [k][j]
   	 lwc1    $f2, 0($t3)
   	 lwc1    $f1, 0($t2)
   	 mul.s     $f2, $f2, $f0
   	 addiu    $s2, $s2, 1   	 #inc j
   	 sub.s    $f1, $f1, $f2
   	 swc1     $f1, 0($t2)
   	 
   	 bne   	 $s2, $a1, sub    #if not end of row
   	 addiu    $t2, $t2, 4   	 #inc elem pointer


   	 addi    $s1, $s1, 1
   	 bne   	 $s1, $a1, sub_row #not last row
   	 nop








   	 addiu    $s0, $s0, 1   	 #inc k
   	 addu   	 $t0, $t0, $s3
   	 addiu    $t0, $t0, 4
   	 bne   	 $s0, $a1, piv_row # jump back if more rows
   	 nop



end_div:

   	 ##
   	 lw   	 $s3, 12($sp)
   	 lw   	 $s2, 8($sp)
   	 lw   	 $s1, 4($sp)
   	 lw   	 $s0, 0($sp)   		 # done restoring registers
   	 addiu    $sp, $sp, 16   	 	# remove stack frame

   	 ##

   	 lw   	 $ra, 0($sp)   		 # done restoring registers
   	 addiu    $sp, $sp, 4   		 # remove stack frame

   	 jr   	 $ra   			 # return from subroutine
   	 nop   				# this is the delay slot associated with all types of jumps

################################################################################
