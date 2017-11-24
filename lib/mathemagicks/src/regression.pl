:-module(regression, []).

%!  logistic_regression(+Xs,+Ys,-Ws) is det.
%
%   Train the weights of a logistic regression model.
%
%   Xs are the features, Ys the labels of training examples. Ws are the
%   learned weights of the logistic regression model.
%
%   This performs binary logistic regression only. For multinomial
%   logistic regression see a suggestively named predicate in this
%   module.
%
logistic_regression(_Xs,_Ys,_Ws).



%!  multinomial_logistic_regression(+Xs,+Ys,-Ws) is det.
%
%   Train the weights of a multinomial logistic regression model.
%
multinomial_logistic_regression(_Xs,_Ys,_Ws).



%!  logistic_regression_prediction(+Ws,+X,+Y) is det.
%
%   Predict the probability of a positive outcome.
%
%   Ws are the weights of a logistic regression model trained with
%   logistic_regression/3. X is a dependent variable with labels from
%   the labels Ys of the trained model.
%
logistic_regression_prediction(_Ws,_X,_Y).
