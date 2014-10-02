A  = [0, 1; -2, -3];
B  = [0; 1];
x0 = [2; 3];
xf = [1; 1];

syms T t tau;

Wr = int(expm(A*(T - tau)) * B * B' * expm(A' * (T - tau)), tau, 0, T);
u  = B' * expm(A' * (T - t)) * inv(Wr) * (xf - expm(A*T) * x0);
phi = expm(A*t) * x0 + int(expm(A*(t - tau)) * B * u, tau, 0, t);

eval(subs(phi, {T, t}, {1, 0}))
eval(subs(phi, {T, t}, {1, 1}))


