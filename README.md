# Dice Task Calculator
Automated analysis of dice tasks for studies on reasoning under uncertainty. Written as research assistant on BMBF project &lt;01UL1906X> "Logic and philosophy of science of reasoning under uncertainty".

This R-subroutine calculates and prints the uncertainty intervals identified by different interpretations of natural language conditionals in dice tasks. In addition, it calculates the connection between antecedent and consequent in terms of different notions of argument strength. The function expects as input two boolean vectors of equal length, representing the color and symbol of each visible side respectively. Vectors shorter than 6 values leave open some uncertainty, leading to probability intervals rather than point values.

Example: The experiment shows 2 sides of a die, the first a white triangle, the second a black square. Every other side is marked with a question mark. The conditional in question is "If the side facing up shows a triangle, the side shows black.". This means that the first side makes the antecedent of the conditional true, but not the consequent, and the second side vice versa. Thus you input (True, False) as the antecedent vector, and (False, True) as the consequent vector, leaving open the remaining four sides.

For more detailed explanations and application examples, see the works of Dr.Dr. Niki Pfeifer.
