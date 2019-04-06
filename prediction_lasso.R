setwd("~/Desktop/BIOS 735/final_project")
load("prenobns.RData")

it_test = itoken(test_use$comment_text,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = test_use$id,
                  progressbar = TRUE)

raw.test.dtm = create_dtm(it_test, vectorizer)

pred <- predict(fit,raw.test.dtm, type = "class")

error <- sum((as.vector(test_labels_use$toxic) - as.numeric(as.character(pred)))^2)
