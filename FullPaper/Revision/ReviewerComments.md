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
    - *We have some data on response time (though imperfect), so we could discuss this briefly if we want to.*
    - *Susan, by imperfect, do you mean that you don't know what participants were doing, because of the missing control of AMT that we would have in lab sessions? - we can state that and move beyond it.*
    - *I think we should want to model the results - the model can go into the appendix, the results in a sentence summary in the main paper.*
- Would results have been different if relative difficulty of clusters vs. trends was different?
    - *Probably... but how to measure that objectively? We'd have to do single-target studies and then compare to dual target results or something... XXX Susan, could you explain to me a bit more, how single target studies would help in this situation? -- I was interested in doing some, but for a different reason (see below).*
    - *Yes the results are different, when the parameters s_C and s_T are changed. We have extended the model accordingly, and added details to the appendix.*
    
## Reviewer 2
- The experiment is very complicated, and the result is not easily interpreted/effectively communicated
    - *That tends to be how real world stuff goes, but I don't disagree with the point*
    - *True, but we can't say it this way. We need to put in some effort to clean up the results.*
- Introduction needs a summary of the main points: preattentive perception, gestalt psychology, and statistical lineups
- The modification to the lineup protocol is a significant and crucial
leap, with only rational justification (no empirical evidence).
There are some assumptions here, rather than truisms or established facts.
    - *True, the modification of the lineup protocol is significant, but I would not call it a leap, but a logical next step for working with the protocol in a different situation. We have to make a better job of listing the assumptions:*
    - *There are some assumptions that we make without empirical back up: does it matter for individuals decision, whether there is one target or two targets? -- yes it will, but it is not at all clear by how much. We could test the effect by using the same lineups in single target lineups by exchanging the second target by a null plot and comparing results (adjusted for 18 versus 19 null plots). When the second target is removed, the first target will(?) attract more decisions. Should the two separate single target lineups overall show about the same number of decisions?*
    - *Other assumptions: in the evaluation, we first checked the significance of each two-target lineup by using simulation-based inference. That doesn't seem particularly problematic - and we don't talk about this in the paper anyways.*
    - *More assumptions: For the model comparing the two targets we are using only those instances, in which at least one of the targets was successfully identified. This means we have fewer data for more difficult decisions. Should we sample? or maybe use weights to see whether it makes a difference for the model? We are in a mtached pair scenario, but we don't use that in the evaluation besides a random effect for the data. We might be able to get a bit more power out here (to decrease the width of the confidence intervals).*
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
- Fig 11 is confusing, given that plot types associated with clustering have a lower probability of correct target evaluations. What is the frequency of cluster vs. trend target selection in a plain graph?
    - *We added the odds for selecting the cluster over the trend target for the plain graph.*
    - *The confusion arises from the problem, that while overall the probability of identifying a target decreases, the odds are still in favor of the clustering target. We might need to add something on the gini index.*
- How do subjective confidence scores correlate with incorrect evaluations or estimated odds as shown in Fig 11?
    - *They don't. Is this consistent with other Mturk studies?*
    - *This is pretty consistent with our findings - there's some discussion in the 'Comparing designs' infovis paper*
- I appreciate the word cloud visualizations of participant comments but I expected a more thorough discussion of their reasoning. 
- Revisit the sentence "...the mentioning of specific colors is indicative of participants' distraction from the intended target towards an imbalance of the color/cluster distribution." for clarification. Also, discuss why the plot types associated with clustering had significantly fewer correct evaluations more thoroughly.
    - *Here's where we should go a bit into the Gini index discussion. In particular, the ellipses seem to highlight the absence of a cluster - figure 15 is screaming that all over.*
- Discussion of additive effects can benefit from the psychophysics literature on integrality (or separability) of perceptual dimensions (Garner & Felfoldy, Integrality of stimulus dimensions in various types of information processing, 1970). 
- Page 19: table 2 -> Table 2
    - *Fixed*