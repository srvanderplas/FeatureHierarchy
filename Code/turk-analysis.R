turk16 <- read.csv(file.choose())

turk16 <- subset(turk16, nick_name != "susan")
turk16 <- subset(turk16, !is.na(pic_id))


pics <- read.csv(file.choose()) # picture_details
pics <- subset(pics, pic_name != "pic_name")
#pics$pic_id <- 1:nrow(pics)

turkall <- merge(turk16, pics, by="pic_id", all.x=T)
table(factor(subset(turkall, pic_name==turkall$pic_name[1])$response_no))
subset(turkall, (pic_name=="Images/Lineups/svgs/311ad7dac474b1fe447089649aa8bc47.svg") & response==20)


targets <- strsplit(as.character(turkall$obs_plot_location), split=", ")
targets <- ldply(targets, function(x) x)
turkall$tr_target <- as.numeric(targets$V1)
turkall$cl_target <- as.numeric(targets$V2)

responses <- strsplit(as.character(turkall$response_no), split=",")
turkall$feedback <- ldply(1:length(responses), 
                          function(i) {
                            resp <- intersect(as.numeric(unlist(responses[i])), c(turkall$tr_target[i], turkall$cl_target[i]))
                            if (length(resp) == 2) return("both")
                            if (length(resp) == 0) return("none")
                            if (all(resp == turkall$tr_target[i])) return("trend")
                            if (all(resp == turkall$tr_cluster[i])) return("cluster")
                            return("shouldn't happen")
                          })$V1

summary(factor(turkall$feedback))
xtabs(~test_param + factor(feedback), data=subset(turkall, param_value==" k-3-sdline-0.25-sdgroup-0.25"))

qplot(feedback, data=subset(turkall, param_value==" k-3-sdline-0.25-sdgroup-0.25"), facets=~test_param)

library(plyr)
turkall$test_param <- gsub("turk16-", "", turkall$test_param)

res <- ddply(turkall, .(test_param, param_value, feedback), summarize, count=length(test_param))
res2 <- ddply(res, .(test_param, param_value), transform, n = sum(count))

library(reshape2)
res2$perc <- with(res2, count/n*100)
dres <- dcast(res2, param_value+test_param~feedback, value.var="perc")
dres[is.na(dres)] <- 0

qplot(trend, cluster, facets=~param_value, colour=test_param, data=dres)