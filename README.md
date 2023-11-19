# Experimental Computational Algebra

Here is my beginner attempt on computing algebraic structures.

## Motivation

Pain point: I'm stupid. While reading [The Arithmetic of Elliptic Curves by Silverman], I saw a lot of computations involving rational maps, order, etc. My goal is to write a helper to help me compute rational maps. (Or at least to see why it doesn't work.)

## Short goal

In `ratmap.pl`, the goal is to check whether a morphism is an isogeny, i.e. whether it maps $\mathcal{O}$ to $\mathcal{O}$.

Example. (III.4.5.)

Consider the following elliptic curves

$$
E_1 \colon y^2 = x^3 + ax^2 + bx,
$$

and

$$
E_2 \colon Y^2 = X^3 - 2aX^2 + rX.
$$

Define a map $\phi \colon (x, y) \to \left(\frac{y^2}{x^2}, \frac{y(b-x^2)}{x^2}\right)$ from $E_1$ to $E_2$. Then $\phi$ is an isogeny.
