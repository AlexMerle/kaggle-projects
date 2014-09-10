library("adabag")

ada = boosting(formula.nosoil, train.orig, mfinal=300)
ada.pred = predict.boosting(ada, valid.orig)
ada.pred$error
ada.pred$confusion


#mean(ada.pred != valid.orig$Cover)
#table(valid.orig$Cover, ada.pred)
