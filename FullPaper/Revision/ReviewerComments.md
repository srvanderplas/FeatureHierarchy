---
title: "Reviewer Comments"
output: word_document
---

*We would like to thank the reviewers for their valuable feedback. We have tried to incorporate all of their suggestions, as detailed below.*
*The biggest change to the paper was to expand the discussion of the underlying models (of accuracy, response time, participants' confidence, and the balance between targets). We added this discussion in an supplemental part and are only highlighting the most relevant results in the paper itself.*

## Reviewer 1
- perceptual theorizing more up to date (attentional processing, guided search models, biased competition models)
    - *We've added some discussion of dual-target search, as well as recent citations on perceptual theory as it applies to statistical graphics.*
- statistical sophistication of participants not addressed
    - *VanderPlas & Hofmann (2016) TVCG citation has been added to provide supporting evidence that statistical training is not a significant predictor of lineup performance.*
- Cite critical evaluation study: [Crump et al (2013)](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0057410)
    - *Thanks for pointing out this paper, we have added it to the discussion.*
- Controlling for speed/accuracy tradeoff. 
    - *We have added a discussion of (log) response time on both the accuracy of lineup evaluations and the balance between targets*
    - *Response time (split into log(response time) and effect of first trial) does not have a significant effect on the decision between cluster and trend (Chisq:4.4763, df:2, p-value=0.1067)*
    - *Response time does have an effect on accuracy - but not in the traditional sense that an increased amount of time spent in the evaluation necessarily leads to a higher accuracy. At first, an increase in response time is associated with a decrease in accuracy, after about 150 seconds, this effect reverses and an increase in response time leads to higher accuracy.*
- Would results have been different if relative difficulty of clusters vs. trends was different?
    - *Yes, the results are different, when the relative difficulty of the lineup changes. Relative difficulty is controlled by the parameters s_C and s_T. We have extended the discussion of the models accordingly, and added details to the supplement.*
    
## Reviewer 2
- The experiment is very complicated, and the result is not easily interpreted/effectively communicated
    - *We have added more structure in the reporting of the results (particularly in the supplement). Hopefully this helps with communicating the results more effectively.*
- Introduction needs a summary of the main points: preattentive perception, gestalt psychology, and statistical lineups
    - *Fixed*
- The modification to the lineup protocol is a significant and crucial leap, with only rational justification (no empirical evidence). There are some assumptions here, rather than truisms or established facts.
    - *True, the modification of the lineup protocol is significant, but I would not call it a leap, rather a logical next step for working with the protocol in a different situation. We have tried to make a better job of explicitly listing the assumptions:*
    - *Having two rather than just one target in the lineup matters for an individual's decision: The theory behind it has its roots in cognition theory: When two targets compete with each other, the dominant target masks the other. As soon as one target is identified, the search usually stops (validated by the fact that in only 0.6% of the evaluations both targets were identified).  When the second target is removed, the first target will attract more decisions.*
    - *From a statistical perspective, for the model comparing the two targets we are using only those instances, in which at least one of the targets was successfully identified. This means we have fewer data for more difficult decisions. However, the different sample sizes are not affecting the parameter estimates, but only show up in their standard errors. The differences are too small to have practical effects (sd(Plain) = 0.1179, sd(Color+Shape+Ellipse) = 0.1310).*
- Reference Fig 2 in the text.
    - *Fixed*
- "Prediction" vs. "error", "line" vs. "trend"
    - *Fixed*. Additional text has been added in the paper to clarify in situations where "error" may be used for formatting sake (e.g. in tables). 
- Figure 6 (f) should be "shape + color + ellipse"?
    - *Fixed*
- Fig 7a should be Fig 7b? (page 16, line 41)
    - *Fig 7a shows the color palette, Fig 7b shows the shapes - we think that all references and descriptions reflect this now.*
- Top of pg 19 - unclear use of "plot" vs. "lineup". Need a clear example of a lineup. Is Figure 6 a lineup?
    - *Figure 6 contains 10 separate lineups. We have added a sentence of clarification in the paper.*
- Confusion about the null hypothesis in Sec 3.2. Are we comparing trend vs. cluster or trend + cues vs. trend - cues or cluster + cues vs. cluster - cues? 
    - *Section 3.2 discusses effects on accuracy (as defined as identification of at least one of the targets in a lineup evaluation), section 3.3 discusses the balance between targets (given at least one target was identified). We have expanded the discussion of the modelling results in the paper to clarify this, and added a lot of extra discussion in a supplement.*
    
    
## Reviewer 3
- Link given to the experiment isn't active.
    - *We have fixed the link to the experiment [now at (https://erichare.shinyapps.io/lineups/)]. We have also added a better description of the experiment in  an extended README file on the github repo at (https://github.com/srvanderplas/FeatureHierarchy)*
- Perceived saliency (ability to identify target) may not mean increased comprehension
    - *This is a valid point:  understanding is based on saliency, but saliency is not sufficient for understanding. Measuring actual comprehension is hard, and we are not claiming to do that with lineups.*
- Task description in a separate subsection, not buried within the participant recruitment subsection
    - *We have added section 2.6, which consists of the task description.*
- Fig 11 is confusing, given that plot types associated with clustering have a lower probability of correct target evaluations. What is the frequency of cluster vs. trend target selection in a plain graph?
    - *There is no such thing as 'the odds' of cluster versus trend selection for a plain graph, because these odds are highly dependent on the actual parameter setting - e.g. for a lineup with a strong cluster signal (i.e. s_C low, and s_T relatively large), the odds of choosing the cluster target are much higher than if the cluster signal is weak. However, the relative log odds (i.e. the odds ratio) between the plot types is unaffected by this, because model (1) does not include any interaction of plot type effect with another co-variate.*
    - *We have added the odds for selecting the cluster over the trend target for the plain graph for one set of parameters to give an indication of the overall odds.*
- How do subjective confidence scores correlate with incorrect evaluations or estimated odds as shown in Fig 11?
    - *A model of self-reported confidence scores is added to the appendix/supplement. Generally, confidence agrees with (perceived) difficulty of the lineup; i.e. for parameter values that allow for more variability in the trend or the clusters, confidence is reported lower. There is one notable exception: for plots with an ellipse confidence is reported higher, corresponding to an (unintended) signal in the cluster assignment of some of the plots.*
    - *This is finding is  consistent with previous results, e.g. there's some discussion in the 'Graphical tests for ... competing designs' paper (Hofmann et al, 2012)*
- I appreciate the word cloud visualizations of participant comments but I expected a more thorough discussion of their reasoning. 
    - *We have added a discussion of the gini impurity as a quantitative measure for the group imbalance that participants' reasoning seems to hint at (in supplement C).*
- Revisit the sentence "...the mentioning of specific colors is indicative of participants' distraction from the intended target towards an imbalance of the color/cluster distribution." for clarification. Also, discuss why the plot types associated with clustering had significantly fewer correct evaluations more thoroughly.
    - *We have re-written the sentence for clarification. Supplement C contains a lot more detail on how the group imbalance (measured in gini impurity as well as the absence of individual ellipses) affected accuracy of lineup evaluations, particularly for lineups with a design tailored for emphasizing clusters.*
- Discussion of additive effects can benefit from the psychophysics literature on integrality (or separability) of perceptual dimensions (Garner & Felfoldy, Integrality of stimulus dimensions in various types of information processing, 1970). 
    - *Thank you for pointing out the reference. We've expanded the discussion accordingly.* 
- Page 19: table 2 -> Table 2
    - *Fixed*