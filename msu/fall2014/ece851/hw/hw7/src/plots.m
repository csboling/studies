function plots
  A  = [0 1; 1 1];
  B  = [1; 0];
  x0 = [1 1]';

  close all;
  
  figure;
  M  = eye(2);
  lqr = optimal_lqr(A, B, eye(2), [0 0]', M, eye(2), [1]);
  [y, t, x] = initial(lqr, x0);
  plot1 = subplot('211');
  plot(t, x(:,1));
  xlabel('Time');
  ylabel('x_1');
  plot2 = subplot('212');
  plot(t, x(:,2));
  xlabel('Time');
  ylabel('x_2');
  title(plot1, 'Problem #2: J_1');
  
  figure;
  M  = (30*eye(2));
  lqr = optimal_lqr(A, B, eye(2), [0 0]', M, eye(2), [1]);
  [y, t, x] = initial(lqr, x0);
  plot1 = subplot('211');
  plot(t, x(:,1));
  xlabel('Time');
  ylabel('x_1');
  plot2 = subplot('212');
  plot(t, x(:,2));
  xlabel('Time');
  ylabel('x_2');
  title(plot1, 'Problem #2: J_2');
end

function loop = optimal_lqr(A, B, C, D, M, Q, R)
  P = are(A, B*inv(R)*B', M'*Q*M);

  forward_dynamics  = ss(A, B, C, D);
  feedback_dynamics = ss(inv(R)*B'*P);
  loop = feedback(forward_dynamics, feedback_dynamics);
end
