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
    - *I think we should want to - the model can go into the appendix, the results in a sentence summary in the main paper.*
- Would results have been different if relative difficulty of clusters vs. trends was different?
    - *Probably... but how to measure that objectively? We'd have to do single-target studies and then compare to dual target results or something...*
    - *Yes the results are different, when the parameters s_C and s_T are changed. We have extended the model accordingly, and added details to the appendix.*
    
## Reviewer 2
- The experiment is very complicated, and the result is not easily interpreted/effectively communicated
    - *That tends to be how real world stuff goes, but I don't disagree with the point*
    - *True, but we can't say it this way. We need to put in some effort to clean up the results.*
- Introduction needs a summary of the main points: preattentive perception, gestalt psychology, and statistical lineups
- Justification of the lineup protocol modification needs expanding - rational justification needs experimental evidence to back it up.
- Reference Fig 2 in the text.
    - *Fixed*
- "Prediction" vs. "error", "line" vs. "trend"
    - *Dangit, I missed a line vs. trend!!!* 
    - *I think I've fixed this, for the most part. Some LaTeX tables may still need help.*
- Figure 6 (f) should be "shape + color + ellipse"?
    - *Fixed*
- Fig 7a should be Fig 7b? (page 16, line 41)
- Top of pg 19 - unclear use of "plot" vs. "lineup". Need a clear example of a lineup. Is Figure 6 a lineup?
    - *Figure 6 contains 10 separate lineups.*
- Confusion about the null hypothesis in Sec 3.2. Are we comparing trend vs. cluster or trend + cues vs. trend - cues or cluster + cues vs. cluster - cues? 
    - *Yes. :-p*
    
## Reviewer 3
- Link given to the experiment isn't active.
 - *Link to https://erichare.shinyapps.io/lineups/ works, but isn't ideal. We're still working on a better description of the experiments.*
 - *One part of the description would be an extended README for the github repo. We need to clean it up, too ...*
- Perceived saliency (ability to identify target) may not mean increased comprehension
    - *valid point, but if you don't see the signal clearly in a non-lineup plot, you can't comprehend whatever it is the creator of the plot was trying to get across...*
- Task description in a separate subsection, not buried within the participant recruitment subsection
- Fig 11 is confusing, given that plot types associated with clustering have a lower probability of correct target evaluations. What is the frequency of cluster vs. trend target selection in a plain graph?
    - *We added the odds for selecting the cluster over the trend target for the plain graph.*
- How do subjective confidence scores correlate with incorrect evaluations or estimated odds as shown in Fig 11?
    - *They don't. Is this consistent with other Mturk studies?*
    - *This is pretty consistent with our findings - there's some discussion in the 'Comparing designs' infovis paper*
- I appreciate the word cloud visualizations of participant comments but I expected a more thorough discussion of their reasoning. 
- Revisit the sentence "...the mentioning of specific colors is indicative of participants' distraction from the intended target towards an imbalance of the color/cluster distribution." for clarification. Also, discuss why the plot types associated with clustering had significantly fewer correct evaluations more thoroughly.
    - *Here's where we should go a bit into the Gini index discussion. In particular, the ellipses seem to highlight the absence of a cluster - figure 15 is screaming that all over.*
- Discussion of additive effects can benefit from the psychophysics literature on integrality (or separability) of perceptual dimensions (Garner & Felfoldy, Integrality of stimulus dimensions in various types of information processing, 1970). 
- Page 19: table 2 -> Table 2
    - *Fixed*