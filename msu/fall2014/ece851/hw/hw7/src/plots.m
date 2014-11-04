function plots
  A  = [0 1; 1 1];
  B  = [1; 0];
  M  = eye(2);
  x0 = [1 1]';

  lqr = optimal_lqr(A, B, eye(2), [0 0]', M, eye(2), -1);
  initial(lqr, x0)
end

loop = function optimal_lqr(A, B, C, D, M, Q, R)
  P = are(A, B*inv(R)*B', M*Q*M);

  forward_dynamics  = ss(A, B, C, D);
  feedback_dynamics = ss(-inv(R)*P);
  loop = feedback(tf(forward_dynamics), tf(feedback_dynamics));
end
