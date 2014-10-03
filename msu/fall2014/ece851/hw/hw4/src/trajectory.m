A  = [-1/2, 0; 0, -1];
B  = [1/2; 1];
x0 = [5; -5];
xf = [0; 0];

syms T s t tau;

Wr = int(expm(A*(T - tau)) * B * B' * expm(A' * (T - tau)), tau, 0, T);
u  = B' * expm(A' * (T - s)) * inv(Wr) * (xf - expm(A*T) * x0);
phi = expm(A*t) * x0 + int(expm(A*(t - s)) * B * u, s, 0, t);


for Tf=[1, 2, 5]
  tindex = [0:0.01:Tf];
  x = eval(subs(phi, {T, t}, {Tf, tindex}));

  h = figure;
  plot(tindex, x(1,:), tindex, x(2,:));
  title(sprintf('ECE 851 Homework #4, Problem #3, T = %d', Tf));
  xlabel('Time (s)');
  ylabel('States');
  legend('x_1', 'x_2');
  fname = sprintf('problem2-%d.pdf', Tf);
  print(h, '-dpdf', fname);
end
