---
title: "Reviewer Comments"
output: html_document
---

## Reviewer 1
- perceptual theorizing more up to date (attentional processing, guided search models, biased competition models)
- statistical sophistication of participants not addressed
    - *Has this been addressed in previous MTurk work?*
    - *Was this addressed by any of Mahbub's demographic studies?*
    - *Let's cite your own study from the Infovis paper*
- Cite critical evaluation study: [Crump et al (2013)](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0057410)
- Controlling for speed/accuracy tradeoff. 
    - *We have added a discussion of (log) response time on both the accuracy of lineup evaluations and the balance between targets*
    - *Response time (split into log(response time) and effect of first trial) does not have a significant effect on the decision between cluster and trend (Chisq:4.4763, df:2, p-value=0.1067)*
    - *Response time does have an effect on accuracy - but not in the traditional sense that an increased amount of time spent in the evaluation necessarily leads to a higher accuracy.*
- Would results have been different if relative difficulty of clusters vs. trends was different?
    - *Yes, the results are different, when the relative difficulty of the lineup changes. Relative difficulty is controlled by the parameters s_C and s_T. We have extended the model accordingly, and added details to the supplement.*
    
## Reviewer 2
- The experiment is very complicated, and the result is not easily interpreted/effectively communicated
    - *We have added more structure in the reporting of the results (particularly in the supplement). Hopefully this helps with communicating the results more effectively.*
- Introduction needs a summary of the main points: preattentive perception, gestalt psychology, and statistical lineups
- The modification to the lineup protocol is a significant and crucial
leap, with only rational justification (no empirical evidence).
There are some assumptions here, rather than truisms or established facts.
    - *True, the modification of the lineup protocol is significant, but I would not call it a leap, rather a logical next step for working with the protocol in a different situation. We have tried to make a better job of explicitly listing the assumptions:*
    - *Does it matter for an individual's decision, whether there is one target or two targets? -- yes it will. The theory behind it has its roots in cognition theory: When two targets compete with each other, the dominant one masks the other. As soon as one target is identified, the search usually stops (validated by the fact that in only 0.6% of the evaluations both targets were identified).  When the second target is removed, the first target will attract more decisions.*
<!--    - *Other assumptions: in the evaluation, we first checked the significance of each two-target lineup by using simulation-based inference. That doesn't seem particularly problematic - and we don't talk about this in the paper anyways.*-->
    - *For the model comparing the two targets we are using only those instances, in which at least one of the targets was successfully identified. This means we have fewer data for more difficult decisions. However, the different sample sizes are not affecting the parameter estimates, but only show up in their standard errors. The differences are too small to have practical effects (sd(Plain) = 0.1179, sd(Color+Shape+Ellipse) = 0.1310).*
- Reference Fig 2 in the text.
    - *Fixed*
- "Prediction" vs. "error", "line" vs. "trend"
    - *Dangit, I missed a line vs. trend!!!* 
    - *I think I've fixed this, for the most part. Some LaTeX tables may still need help.*
- Figure 6 (f) should be "shape + color + ellipse"?
    - *Fixed*
- Fig 7a should be Fig 7b? (page 16, line 41)
- Top of pg 19 - unclear use of "plot" vs. "lineup". Need a clear example of a lineup. Is Figure 6 a lineup?
    - *Figure 6 contains 10 separate lineups. That needs an additional sentence of clarification in the paper.*
- Confusion about the null hypothesis in Sec 3.2. Are we comparing trend vs. cluster or trend + cues vs. trend - cues or cluster + cues vs. cluster - cues? 
    - *Yes. :-p* *XXX Yes, in all of the above?*
    
    
## Reviewer 3
- Link given to the experiment isn't active.
    - *Link to https://erichare.shinyapps.io/lineups/ works, but isn't ideal. We're still working on a better description of the experiments.*
    - *One part of the description would be an extended README for the github repo. We need to clean it up, too ...*
- Perceived saliency (ability to identify target) may not mean increased comprehension
    - *valid point, but if you don't see the signal clearly in a non-lineup plot, you can't comprehend whatever it is the creator of the plot was trying to get across...*
    - *Is there some cognitive research on saliency versus comprehension? - There was an Infovis paper in 2013 and 2016 on memorability of graphs.*
- Task description in a separate subsection, not buried within the participant recruitment subsection
    - *that might help, I agree*
- Fig 11 is confusing, given that plot types associated with clustering have a lower probability of correct target evaluations. What is the frequency of cluster vs. trend target selection in a plain graph?
    - *There is no such thing as 'the odds' of cluster versus trend selection for a plain graph, because these odds are highly dependent on the actual parameter setting - e.g. for a lineup with a strong cluster signal (i.e. s_C low, and s_T relatively large), the odds of choosing the cluster target are much higher than if the cluster signal is weak. However, the relative log odds (i.e. the odds ratio) between the plot types is unaffected by this, because model (1) does not include any interaction of plot type effect with another co-variate.*
    - *We have added the odds for selecting the cluster over the trend target for the plain graph for one set of parameters to give an indication of the overall odds.*
    - *The confusion arises from the problem, that while overall the probability of identifying a target decreases, the odds are still in favor of the clustering target. We might need to add something on the gini index.*
- How do subjective confidence scores correlate with incorrect evaluations or estimated odds as shown in Fig 11?
    - *They don't. Is this consistent with other Mturk studies?*
    - *A model of self-reported confidence scores is added to the appendix/supplement. Generally, confidence agrees with (perceived) difficulty of the lineup; i.e. for parameter values that allow for more variability in the trend or the clusters, confidence is reported lower. There is one notable exception: for plots with an ellipse confidence is reported higher, corresponding to an (unintended) signal in the cluster assignment of some of the plots.*
    - *This is pretty consistent with our findings - there's some discussion in the 'Comparing designs' infovis paper*
- I appreciate the word cloud visualizations of participant comments but I expected a more thorough discussion of their reasoning. 
- Revisit the sentence "...the mentioning of specific colors is indicative of participants' distraction from the intended target towards an imbalance of the color/cluster distribution." for clarification. Also, discuss why the plot types associated with clustering had significantly fewer correct evaluations more thoroughly.
    - *Here's where we should go a bit into the Gini index discussion. In particular, the ellipses seem to highlight the absence of a cluster - figure 15 is screaming that all over.*
- Discussion of additive effects can benefit from the psychophysics literature on integrality (or separability) of perceptual dimensions (Garner & Felfoldy, Integrality of stimulus dimensions in various types of information processing, 1970). 
- Page 19: table 2 -> Table 2
    - *Fixed*