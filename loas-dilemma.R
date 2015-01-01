## LOAS Dilemma

## Function is calculating utility given outcome, according to Prospect Theory.
# 
# v(x) = x^a, if x > 0
# v(x) = -m(-x^a), if x < 0
# v(x) = 0, if x = 0
# (with a typical a = 0.88 and m = 2.25)
# 
# This process has three major characteristics:
# - Reference level dependence: An individual views consequences (monetary or other) in terms of changes from the reference level, which is usually that individual's status quo.
# - Gain and loss satiation: The values of the outcomes for both positive and negative consequences of the choice have the diminishing returns characteristic. The a term in the value function equation captures the marginally decreasing aspect of the function. Empirical studies estimate that a is typically equal to approximately .88 and always less than 1.00. When the exponent a < 1.00, the curve will accelerate negatively (if a = 1.00, the function would be linear; and if a > 1.00, if would accelerate positively).
# - Loss aversion: The resulting value function is steeper for losses than for gains; losing $100 produces more pain than gaining $100 produces pleasure. The coefficient ? indexes the difference in slopes of the positive and negative arms of the value function. A typical estimate of ? is 2.25, indicating that losses are approximately twice as painful and gains are pleasurable. (If ? = 1.00, the gains and losses would have equal slopes; if ? < 1.00, gains would weigh more heavily than losses.)’
# Hastie and Dawes (2001, p. 294)

u <- function(outcome, alpha=0.88, lambda=2.25) {
	if (outcome > 0) {
		utility <-  outcome ^ alpha
	} else if (outcome < 0) {
		utility <- -lambda * ((-outcome) ^ alpha)
	} else {
		utility <- 0
	}
	return(utility)
}

# Plot utility function
oMin = -10
oMax = 10
outcome = seq(oMin, oMax, by=0.01)
utility = sapply(outcome, u)
plot(	outcome , utility, 
	type = 'l', 
	main = "Holistic vs Reductionistic Calculus", 
	sub = "Prospect Theory Prediction"
)
abline(h = 0, v = 0, col = "black")
grid()

## Notation
# f - failure outcome
# r - return to old outcome
# n - move to new outcome

## Assumptions
## A.1 - Failure is the worst outcome, 
# with return being better and move to new being the best.
# f < r < n
## A.2 - In absolute terms, the failure gets the highest outcome impact, 
# followed by move to new, followed by return to old.
# |r| < |n| < |f|

## A'.3 - Let's assign some values to f, r and n, that comply with A.1 and A.2
f = -10
r = 2
n = 5

## Problem definition: Calculate difference in utilities for the following 2 scenarios:
# 1) Failure, followed by moving to a new profession (S1)
# 2) Failure, followed by returning to an old profession (S2)

## Those 2 scenarios can be evaluated using 2 different approaches:
# 1) Holistic (look at both outcomes as a whole when evaluating difference) (H)
# 2) Reductionistic (the difference of outcomes 
#    comes only from the difference of the part of the outcome that is different) (R)

## Holistic calculus 
## Frame of reference: before failure.
# [S1 - S2] = utility(S1) - utility(S2) 
# duH       = utility(f + n) - utility(f + r)

duH = u(f + n) - u(f + r)

points(f + n, u(f + n), col="red", pch='N')
points(f + r, u(f + r), col="red", pch='R')

abline(h = list(u(f + n), u(f + r)), col = "red")
text(oMin, mean(c(u(f + n), u(f + r))), paste("Holistic [S1 - S2] = ", duH), col = "red", pos = 4, cex = 0.8)

## Reductionistic calculus 
## Frame of reference: after failure, since failure is the same in both scenarios.
# [S1 - S2] = [(f + n) - (f + r)] = [n - r]
# duR       = utility(n) - utility(r)

duR = u(n) - u(r)

points(n, u(n), col="blue", pch='N')
points(r, u(r), col="blue", pch='R')

abline(h = list(u(n), u(r)), col = "blue")
text(oMin, mean(c(u(n), u(r))), paste("Reduct [S1 - S2] = ", duR), col = "blue", pos = 4, cex = 0.8)

