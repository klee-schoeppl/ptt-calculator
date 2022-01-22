#### 1. Overview
Automated analysis of probabilistic truth-table tasks for studies on reasoning under uncertainty. Written as research assistant on [BMBF project <01UL1906X>](https://homepages.uni-regensburg.de/~pfn23853/LogWissUns.html) of Dr. Dr. Niki Pfeifer. At this point only so-called dice tasks — see the third section — have been fully implemented, but a more general analysis is planned.

***
#### 2. PTT-Calculator
The current - unfinished - version can be accessed for testing in the browser [here](https://leon-schoeppl.shinyapps.io/ptt-calculator/), hosted by *shinyapps.io*.

***
#### 3. Dice Task Subroutine
Dice tasks constitute that subset of probabilistic truth table tasks which is restricted to 6 sides of a die, each featuring a symbol and a color. Participants ought to evaluate the probability intervals of conditional sentences like: “If the side facing up shows *antecedent* (A), then the side shows *consequent* (C).”. *A* and *C* are independent, so if one is a symbol, the other is a color (and vice versa). In-completeness of probabilistic knowledge is introduced by leaving some sides of the dice blank, which is marked by *?*.

This R-subroutine calculates and prints the uncertainty intervals identified by different interpretations of natural language conditionals in dice tasks. In addition, it calculates the connection between antecedent and consequent in terms of different notions of argument strength. The function expects as input two boolean vectors of equal length, representing the color and symbol of each visible side, respectively. Vectors shorter than 6 values leave open some uncertainty, leading to probability intervals rather than point values. Written using R version 4.1.0 (2021-05-18).

Example: The experiment shows 2 sides of a die, the first a white triangle, the second a black square. Every other side is marked with a question mark. The conditional in question is “If the side facing up shows a triangle, the side shows black.”. This means that the first side makes the antecedent of the conditional true, but not the consequent, and the second side vice versa. Thus, you input (True, False) as the antecedent vector, and (False, True) as the consequent vector, leaving open the remaining four sides.

For more detailed explanations and application examples, see Pfeifer 2013a, Pfeifer 2013b, Pfeifer & Tulkki 2017.

***
#### 4. Dice Task Shiny-App
This little Shiny application does the exact same thing as the Dice Task Subroutine, but comes with a UI. Quickly accessible for testing in the browser [here](https://leon-schoeppl.shinyapps.io/dicetask/), hosted by *shinyapps.io*. If you want to actually work with the software, please download it and run it locally.

***
#### 5. References
* Hartmann, S., & Sprenger, J. (2010). Bayesian epistemology.
* Pfeifer, N. (2013a). On argument strength. In F. Zenker (Ed.), *Bayesian argumentation. The practical side of probability* (p. 185-193). Dordrecht: Synthese Library Vol. 362 (Springer).
* Pfeifer, N. (2013b). The new psychology of reasoning: A mental probability logical perspective. *Thinking & Reasoning*, 19(3-4), 329-345.
* Pfeifer, N. & Tulkki, L. (2017). Abductive, causal, and counterfactual conditionals under incomplete probabilistic knowledge. In Gunzelmann, G., Howes, A., Tenbrink, T., &, Davelaar, E. (Eds.). *Proceedings of the 39th Cognitive Science Society Meeting* (p. 2888-2893).

