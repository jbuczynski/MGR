
phi1= matrix(c(-1,2,4,0,-2,3,4,0,1,-3,4,0,1,-2,-4,0,2,-3,-4,0,-1,3,-4,0,1,2,3,0),ncol=4,byrow=TRUE)
#phi1 <- read.table("aim-100-1_6-no-1.cnf.txt", skip=11)

fitness <- function(v) {

	results = apply(phi1, 1, function(row) {
		computePosition <- function(index, row) {
			#if negation
			if(row[index] < 0) {
				#check that v value 0 then ret 1
				if(v[abs(row[index])] == 0) 
				{
					#print( 1 )
					1
				} else {
				 0
				}
			} 
			#if greater than 0 chec value of v
			else if(row[index] > 0){
				if(v[row[index]] > 0){
					#print( 1 )
					1
				} 
				else {
					0
				}
			} 
			#if zero end of row
			else {
				0
			}

		}
		#print(computePosition(1, row))
		#print(computePosition(2, row))
		#print(computePosition(3, row))
		if((computePosition(1, row) + computePosition(2, row) + computePosition(3, row))  > 0) {
			#print(1)
		 	1
		} else {
			0
		}
	})
	-sum(results)
	#suma
}

 GAmodel <- rbga.bin(size = 4, popSize = 200, iters = 100, mutationChance = 0.01, elitism = T, evalFunc = fitness)
 summary(GAmodel, echo=TRUE)
