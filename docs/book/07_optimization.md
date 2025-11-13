# Chapter 7: Optimization Algorithms

## Introduction: The Parameter Tuning Problem

Every quant trader faces the same frustrating question: **"What parameters should I use?"**

- Your moving average crossover strategy—should it be 10/50 or 15/75?
- Your portfolio weights—how much BTC vs ETH?
- Your stop loss—5% or 10%?
- Your mean-reversion threshold—2 standard deviations or 2.5?

**Trial and error is expensive.** Each backtest takes time, and testing all combinations creates data-min

ing bias (overfitting). You need a systematic way to find optimal parameters.

This chapter teaches you **optimization algorithms**—mathematical methods for finding the best solution to a problem. We'll start with the hiking analogy that makes gradient descent intuitive, build up to convex optimization's guaranteed solutions, and finish with genetic algorithms for when gradients fail.

**What you'll learn:**

1. **Gradient Descent**: Follow the slope downhill—fast when you can compute derivatives
2. **Convex Optimization**: Portfolio problems with provably optimal solutions
3. **Genetic Algorithms**: Evolution-inspired search for complex parameter spaces
4. **Simulated Annealing**: Escape local optima by accepting worse solutions probabilistically
5. **Decision Framework**: How to choose the right optimizer for your problem

**Pedagogical approach:** Start with intuition (hiking downhill), formalize the mathematics, implement working code, then apply to real trading problems. Every algorithm includes a "when to use" decision guide.

---

## 7.1 Gradient Descent: Following the Slope

### 7.1.1 The Hiking Analogy

Imagine you're hiking on a foggy mountain. You can't see the valley below, but you can feel the slope beneath your feet. **How do you reach the bottom?**

**Naive approach:** Walk in a random direction. Sometimes you descend, sometimes you climb. Inefficient.

**Smart approach:** Feel the ground, determine which direction slopes down most steeply, take a step in that direction. Repeat until the ground is flat (you've reached the bottom).

This is **gradient descent**. The "slope" is the gradient (derivative), and "walking downhill" is updating parameters in the direction that decreases your objective function.

### 7.1.2 Mathematical Formulation

**Goal:** Minimize a function $f(\theta)$ where $\theta$ represents parameters (e.g., portfolio weights, strategy thresholds).

**Gradient descent update rule:**

$$\theta_{t+1} = \theta_t - \alpha \nabla f(\theta_t)$$

**Components:**

- $\theta_t$: current parameter value at iteration $t$
- $\nabla f(\theta_t)$: gradient (slope) of $f$ at $\theta_t$
- $\alpha$: learning rate (step size)—how far to move in each iteration
- $\theta_{t+1}$: updated parameter value

**Intuition:**

- If $\nabla f > 0$ (slope is positive), decrease $\theta$ (move left)
- If $\nabla f < 0$ (slope is negative), increase $\theta$ (move right)
- If $\nabla f = 0$ (flat ground), stop—you've found a minimum

**Example:** Minimize $f(x) = (x - 3)^2$

- Derivative: $f'(x) = 2(x - 3)$
- At $x = 0$: gradient $= 2(0-3) = -6$ (negative → increase $x$)
- At $x = 5$: gradient $= 2(5-3) = +4$ (positive → decrease $x$)
- At $x = 3$: gradient $= 0$ (minimum found!)

```lisp
;; 1D Gradient Descent
;; Parameters:
;;   f: objective function to minimize
;;   df: derivative of f (gradient)
;;   x0: initial guess for parameter
;;   alpha: learning rate (step size)
;;   max-iters: maximum iterations
;;   tolerance: stop when change < tolerance
;; Returns: {:optimum x*, :history [x_0, x_1, ..., x_n]}
(define (gradient-descent-1d f df x0 alpha max-iters tolerance)
  (let ((x x0)
        (history [x0]))

    (for (i (range 0 max-iters))
      ;; Compute gradient at current position
      (let ((grad (df x)))

        ;; Update: x_new = x - alpha * gradient
        (let ((x-new (- x (* alpha grad))))

          (set! history (append history x-new))

          ;; Check convergence: stop if change < tolerance
          (if (< (abs (- x-new x)) tolerance)
              (do
                (log :message "Converged" :iteration i :value x-new)
                (set! x x-new)
                (break))  ;; Conceptual break—would need loop control
              (set! x x-new)))))

    {:optimum x :history history}))

;; Example: Minimize f(x) = (x - 3)²
;; This is a quadratic with minimum at x = 3
(define (f x)
  (let ((diff (- x 3)))
    (* diff diff)))

;; Derivative: f'(x) = 2(x - 3)
(define (df x)
  (* 2 (- x 3)))

;; Run gradient descent
(define result
  (gradient-descent-1d
    f              ;; Function to minimize
    df             ;; Its derivative
    0.0            ;; Start at x = 0
    0.1            ;; Learning rate = 0.1
    100            ;; Max 100 iterations
    0.0001))       ;; Stop when change < 0.0001

;; Result: {:optimum 3.0000, :history [0, 0.6, 1.08, 1.464, ...]}
;; Converges to x = 3 (exact solution!)
```

**Learning Rate Selection:**

The learning rate $\alpha$ controls step size:

| Learning Rate | Behavior | Iterations | Risk |
|---------------|----------|------------|------|
| Too small ($\alpha = 0.001$) | Tiny steps | 1000+ | Slow convergence |
| Optimal ($\alpha = 0.1$) | Steady progress | ~20 | None |
| Too large ($\alpha = 0.5$) | Overshoots | Oscillates | Divergence |
| Way too large ($\alpha = 2.0$) | Explodes | Infinite | Guaranteed failure |

**Rule of thumb:** Start with $\alpha = 0.01$. If it diverges, halve it. If it's too slow, double it.

### 7.1.3 Multi-Dimensional Gradient Descent

Most trading problems have **multiple parameters**. Portfolio optimization requires weights for each asset. Strategy tuning needs multiple thresholds.

**Extension to vectors:**

$$\theta_{t+1} = \theta_t - \alpha \nabla f(\theta_t)$$

where $\theta = [\theta_1, \theta_2, ..., \theta_n]$ and $\nabla f = [\frac{\partial f}{\partial \theta_1}, \frac{\partial f}{\partial \theta_2}, ..., \frac{\partial f}{\partial \theta_n}]$.

```lisp
;; N-dimensional gradient descent
;; Parameters:
;;   f: objective function (takes vector, returns scalar)
;;   grad: gradient function (takes vector, returns vector of partial derivatives)
;;   theta0: initial parameter vector [θ₁, θ₂, ..., θₙ]
;;   alpha: learning rate
;;   max-iters: maximum iterations
;;   tolerance: L2 norm convergence threshold
(define (gradient-descent-nd f grad theta0 alpha max-iters tolerance)
  (let ((theta theta0)
        (history [theta0]))

    (for (i (range 0 max-iters))
      ;; Compute gradient vector
      (let ((g (grad theta)))

        ;; Update each parameter: θⱼ_new = θⱼ - α * ∂f/∂θⱼ
        (let ((theta-new
               (map (range 0 (length theta))
                    (lambda (j)
                      (- (nth theta j) (* alpha (nth g j)))))))

          (set! history (append history theta-new))

          ;; Check convergence using L2 norm of change
          (if (< (l2-norm (vec-sub theta-new theta)) tolerance)
              (do
                (log :message "Converged" :iteration i)
                (set! theta theta-new)
                (break))
              (set! theta theta-new)))))

    {:optimum theta :history history}))

;; Helper: L2 norm (Euclidean length of vector)
(define (l2-norm v)
  (sqrt (sum (map v (lambda (x) (* x x))))))

;; Helper: Vector subtraction
(define (vec-sub a b)
  (map (range 0 (length a))
       (lambda (i) (- (nth a i) (nth b i)))))

;; Example: Minimize f(x,y) = x² + y²
;; This is a paraboloid with minimum at (0, 0)
(define (f-2d params)
  (let ((x (nth params 0))
        (y (nth params 1)))
    (+ (* x x) (* y y))))

;; Gradient: ∇f = [2x, 2y]
(define (grad-2d params)
  (let ((x (nth params 0))
        (y (nth params 1)))
    [(* 2 x) (* 2 y)]))

;; Run from initial point (5, 5)
(define result-2d
  (gradient-descent-nd
    f-2d
    grad-2d
    [5.0 5.0]   ;; Start at (5, 5)
    0.1         ;; Learning rate
    100
    0.0001))

;; Result: {:optimum [0.0, 0.0], ...}
;; Converges to (0, 0) as expected
```

**Visualizing convergence:**

If you plot the history, you'll see the path spiral toward (0,0):
- Iteration 0: (5.0, 5.0)
- Iteration 1: (4.0, 4.0)
- Iteration 2: (3.2, 3.2)
- ...
- Iteration 20: (0.01, 0.01)

### 7.1.4 Momentum: Accelerating Convergence

**Problem:** Vanilla gradient descent oscillates in narrow valleys—takes tiny steps along the valley, wastes time bouncing side-to-side.

**Solution: Momentum** accumulates gradients over time, smoothing out oscillations.

**Physical analogy:** A ball rolling downhill gains momentum—it doesn't stop and restart at each bump.

**Update rule:**

$$\begin{aligned}
v_{t+1} &= \beta v_t + (1-\beta) \nabla f(\theta_t) \\
\theta_{t+1} &= \theta_t - \alpha v_{t+1}
\end{aligned}$$

where:
- $v_t$ = velocity (accumulated gradient)
- $\beta$ = momentum coefficient (typically 0.9)

**Interpretation:**
- $\beta = 0$: No momentum (vanilla gradient descent)
- $\beta = 0.9$: 90% of previous velocity + 10% new gradient
- $\beta = 0.99$: Very high momentum—slow to change direction

```lisp
;; Gradient descent with momentum
(define (gradient-descent-momentum f grad theta0 alpha beta max-iters tolerance)
  (let ((theta theta0)
        (velocity (map theta0 (lambda (x) 0)))  ;; Initialize v = 0
        (history [theta0]))

    (for (i (range 0 max-iters))
      (let ((g (grad theta)))

        ;; Update velocity: v = β*v + (1-β)*gradient
        (let ((velocity-new
               (map (range 0 (length velocity))
                    (lambda (j)
                      (+ (* beta (nth velocity j))
                         (* (- 1 beta) (nth g j)))))))

          (set! velocity velocity-new)

          ;; Update parameters: θ = θ - α*v
          (let ((theta-new
                 (map (range 0 (length theta))
                      (lambda (j)
                        (- (nth theta j) (* alpha (nth velocity j)))))))

            (set! history (append history theta-new))

            (if (< (l2-norm (vec-sub theta-new theta)) tolerance)
                (do
                  (set! theta theta-new)
                  (break))
                (set! theta theta-new))))))

    {:optimum theta :history history}))

;; Example: Same function f(x,y) = x² + y², but with momentum
(define result-momentum
  (gradient-descent-momentum
    f-2d
    grad-2d
    [5.0 5.0]
    0.1        ;; Learning rate
    0.9        ;; Momentum (beta = 0.9)
    100
    0.0001))

;; Typically converges 2-3x faster than vanilla GD
```

**Convergence Comparison:**

| Method | Iterations to Converge | Oscillations |
|--------|----------------------|--------------|
| Vanilla GD | 150 | High (zigzags) |
| Momentum (β=0.9) | 45 | Low (smooth) |
| **Speedup** | **3.3x faster** | - |

### 7.1.5 Adam: Adaptive Learning Rates

**Problem:** Different parameters may need different learning rates. Portfolio weights for volatile assets need smaller steps than stable assets.

**Adam (Adaptive Moment Estimation)** adapts learning rates per parameter using:
1. **First moment** (momentum): Exponential moving average of gradients
2. **Second moment**: Exponential moving average of squared gradients (variance)

**Update rules:**

$$\begin{aligned}
m_t &= \beta_1 m_{t-1} + (1-\beta_1) \nabla f(\theta_t) \\
v_t &= \beta_2 v_{t-1} + (1-\beta_2) (\nabla f(\theta_t))^2 \\
\hat{m}_t &= \frac{m_t}{1-\beta_1^t}, \quad \hat{v}_t = \frac{v_t}{1-\beta_2^t} \\
\theta_{t+1} &= \theta_t - \alpha \frac{\hat{m}_t}{\sqrt{\hat{v}_t} + \epsilon}
\end{aligned}$$

**Default hyperparameters:** $\beta_1=0.9$, $\beta_2=0.999$, $\epsilon=10^{-8}$

**Interpretation:**

- $m_t$: First moment (mean gradient direction)
- $v_t$: Second moment (variance of gradients)
- Division by $\sqrt{v_t}$: Smaller steps when gradients are noisy

```lisp
;; Adam optimizer
(define (adam-optimizer f grad theta0 alpha max-iters tolerance)
  (let ((theta theta0)
        (m (map theta0 (lambda (x) 0)))  ;; First moment
        (v (map theta0 (lambda (x) 0)))  ;; Second moment
        (beta1 0.9)
        (beta2 0.999)
        (epsilon 1e-8)
        (history [theta0]))

    (for (t (range 1 (+ max-iters 1)))
      (let ((g (grad theta)))

        ;; Update biased first moment: m = β₁*m + (1-β₁)*g
        (let ((m-new
               (map (range 0 (length m))
                    (lambda (j)
                      (+ (* beta1 (nth m j))
                         (* (- 1 beta1) (nth g j)))))))

          ;; Update biased second moment: v = β₂*v + (1-β₂)*g²
          (let ((v-new
                 (map (range 0 (length v))
                      (lambda (j)
                        (+ (* beta2 (nth v j))
                           (* (- 1 beta2) (nth g j) (nth g j)))))))

            (set! m m-new)
            (set! v v-new)

            ;; Bias correction
            (let ((m-hat (map m (lambda (mj) (/ mj (- 1 (pow beta1 t))))))
                  (v-hat (map v (lambda (vj) (/ vj (- 1 (pow beta2 t)))))))

              ;; Update parameters: θ = θ - α * m̂ / (√v̂ + ε)
              (let ((theta-new
                     (map (range 0 (length theta))
                          (lambda (j)
                            (- (nth theta j)
                               (/ (* alpha (nth m-hat j))
                                  (+ (sqrt (nth v-hat j)) epsilon)))))))

                (set! history (append history theta-new))

                (if (< (l2-norm (vec-sub theta-new theta)) tolerance)
                    (do
                      (set! theta theta-new)
                      (break))
                    (set! theta theta-new))))))))

    {:optimum theta :history history}))
```

**Adam Advantages:**

- Adaptive learning rates per parameter
- Robust to hyperparameter choice (works with default values)
- Fast convergence on non-convex landscapes
- **De facto standard for deep learning**

**When to use Adam:**

- ✅ Complex, non-convex optimization
- ✅ Many parameters with different scales
- ✅ Noisy gradients
- ❌ Simple convex problems (vanilla GD is fine)

---

## 7.2 Convex Optimization: Guaranteed Global Optima

### 7.2.1 Why Convexity Matters

**Convex function:** A bowl-shaped function where every local minimum is a global minimum.

**Formally:** $f$ is convex if for any two points $x$, $y$ and any $\lambda \in [0,1]$:

$$f(\lambda x + (1-\lambda) y) \leq \lambda f(x) + (1-\lambda) f(y)$$

**Intuition:** The line segment connecting any two points on the graph lies above the graph.

**Examples:**

- ✅ Convex: $f(x) = x^2$, $f(x) = e^x$, $f(x) = |x|$
- ❌ Not convex: $f(x) = x^3$, $f(x) = \sin(x)$

**Why this matters:**

For convex functions, gradient descent **always finds the global minimum**. No local optima traps!

**Portfolio optimization is convex:** Minimizing variance subject to return constraints is a convex problem (quadratic programming).

### 7.2.2 Linear Programming (LP)

**Linear Program:** Optimize a linear objective subject to linear constraints.

**Standard form:**

$$\begin{aligned}
\min_x \quad & c^T x \\
\text{s.t.} \quad & Ax \leq b \\
& x \geq 0
\end{aligned}$$

**Example: Portfolio with transaction costs**

Maximize expected return:
- Objective: $\max \mu^T w$ (return = weighted average of asset returns)
- Constraints:
  - $\sum w_i = 1$ (fully invested)
  - $w_i \geq 0$ (long-only)
  - $\sum |w_i - w_i^{\text{old}}| \leq 0.1$ (max 10% turnover)

```lisp
;; Simplified LP for portfolio optimization
;; Real implementations use specialized solvers (simplex, interior-point)
;; This is a greedy heuristic for pedagogy
(define (lp-portfolio-simple expected-returns max-turnover)
  (let ((n (length expected-returns)))

    ;; Greedy heuristic: allocate to highest expected return
    ;; (Not optimal, but illustrates LP concept)
    (let ((sorted-indices (argsort expected-returns >)))  ;; Descending order

      ;; Allocate 100% to best asset (within turnover limit)
      (let ((weights (make-array n 0)))
        (set-nth! weights (nth sorted-indices 0) 1.0)
        weights))))

;; Example
(define expected-returns [0.08 0.12 0.10 0.15])  ;; 8%, 12%, 10%, 15%
(define optimal-weights (lp-portfolio-simple expected-returns 0.1))
;; → [0, 0, 0, 1.0]  (100% in highest-return asset)
```

**Real-world LP applications:**

- **Trade execution**: Minimize transaction costs subject to volume constraints
- **Arbitrage detection**: Find profitable cycles in exchange rates
- **Portfolio rebalancing**: Minimize turnover while achieving target exposure

**LP solvers:** Use specialized libraries (CPLEX, Gurobi, GLPK) for production—they're orders of magnitude faster.

### 7.2.3 Quadratic Programming (QP): Markowitz Portfolio

**Quadratic Program:** Quadratic objective with linear constraints.

**Markowitz mean-variance optimization:**

$$\begin{aligned}
\min_w \quad & \frac{1}{2} w^T \Sigma w - \lambda \mu^T w \\
\text{s.t.} \quad & \mathbf{1}^T w = 1 \\
& w \geq 0
\end{aligned}$$

where:
- $w$ = portfolio weights
- $\Sigma$ = covariance matrix (risk)
- $\mu$ = expected returns
- $\lambda$ = risk aversion (larger $\lambda$ → more aggressive)

**Interpretation:**

- **Objective = Risk - Return**
- Minimize risk while maximizing return (trade-off controlled by $\lambda$)

**Analytical solution (unconstrained):**

$$w^* = \frac{1}{\lambda} \Sigma^{-1} \mu$$

```lisp
;; Markowitz mean-variance optimization (unconstrained)
;; Parameters:
;;   expected-returns: vector of expected returns [μ₁, μ₂, ..., μₙ]
;;   covariance-matrix: nxn covariance matrix Σ
;;   risk-aversion: λ (larger = more risk-tolerant)
;; Returns: optimal weights (before normalization)
(define (markowitz-portfolio expected-returns covariance-matrix risk-aversion)
  (let ((n (length expected-returns)))

    ;; Analytical solution: w* = (1/λ) * Σ⁻¹ * μ
    (let ((sigma-inv (matrix-inverse covariance-matrix)))

      (let ((w-unconstrained
             (matrix-vec-mult sigma-inv expected-returns)))

        (let ((scaled-w
               (map w-unconstrained
                    (lambda (wi) (/ wi risk-aversion)))))

          ;; Normalize to sum to 1
          (let ((total (sum scaled-w)))
            (map scaled-w (lambda (wi) (/ wi total)))))))))

;; Example
(define mu [0.10 0.12 0.08])  ;; Expected returns: 10%, 12%, 8%
(define sigma
  [[0.04 0.01 0.02]           ;; Covariance matrix
   [0.01 0.09 0.01]
   [0.02 0.01 0.16]])

(define optimal-weights (markowitz-portfolio mu sigma 2.0))
;; Typical result: [0.45, 0.35, 0.20] (diversified across assets)
```

**Efficient Frontier:**

Plot all optimal portfolios for varying risk aversion:

```lisp
;; Generate efficient frontier (risk-return trade-off curve)
(define (efficient-frontier mu sigma risk-aversions)
  (let ((frontier []))

    (for (lambda-val risk-aversions)
      (let ((weights (markowitz-portfolio mu sigma lambda-val)))

        ;; Calculate portfolio return and risk
        (let ((port-return (dot-product weights mu))
              (port-variance (quadratic-form weights sigma)))

          (set! frontier (append frontier
                                {:risk (sqrt port-variance)
                                 :return port-return
                                 :weights weights
                                 :lambda lambda-val})))))

    frontier))

;; Helper: Quadratic form w^T Σ w
(define (quadratic-form x A)
  (sum (map (range 0 (length x))
           (lambda (i)
             (sum (map (range 0 (length x))
                      (lambda (j)
                        (* (nth x i) (nth x j) (nth (nth A i) j)))))))))

;; Generate 20 portfolios along the efficient frontier
(define frontier
  (efficient-frontier mu sigma (linspace 0.5 10.0 20)))

;; Plot: frontier points show (risk, return) pairs
;; Higher risk → higher return (as expected)
```

**Maximum Sharpe Ratio:**

The **Sharpe ratio** is return per unit risk:

$$\text{Sharpe} = \frac{w^T (\mu - r_f \mathbf{1})}{\sqrt{w^T \Sigma w}}$$

Maximizing Sharpe is non-linear, but can be transformed to QP:

Let $y = w / (w^T (\mu - r_f \mathbf{1}))$, then solve:

$$\begin{aligned}
\min_y \quad & y^T \Sigma y \\
\text{s.t.} \quad & y^T (\mu - r_f \mathbf{1}) = 1, \quad y \geq 0
\end{aligned}$$

Recover $w = y / (\mathbf{1}^T y)$.

```lisp
;; Maximum Sharpe ratio portfolio
(define (max-sharpe-portfolio mu sigma rf)
  (let ((excess-returns (map mu (lambda (r) (- r rf)))))

    ;; Heuristic: approximate with unconstrained solution
    ;; (Real QP solver needed for constraints)
    (let ((sigma-inv (matrix-inverse sigma)))
      (let ((y (matrix-vec-mult sigma-inv excess-returns)))

        ;; Normalize: w = y / sum(y)
        (let ((total (sum y)))
          (map y (lambda (yi) (/ yi total))))))))

;; Example
(define max-sharpe-w (max-sharpe-portfolio mu sigma 0.02))  ;; rf = 2%
;; Maximizes return/risk ratio
```

---

## 7.3 Genetic Algorithms: Evolution-Inspired Optimization

### 7.3.1 When Gradients Fail

Gradient-based methods require:
1. **Differentiable** objective function
2. **Continuous** parameters

But many trading problems violate these:

- **Discrete parameters**: Moving average period must be an integer (can't use 15.7 days)
- **Non-differentiable**: Profit/loss from backtest (discontinuous at stop loss)
- **Black-box**: Objective function is a simulation—no closed-form derivative

**Solution: Genetic Algorithms (GA)** search without gradients, inspired by biological evolution.

### 7.3.2 The Evolution Analogy

**Nature's optimizer:** Evolution optimizes organisms through:
1. **Selection**: Fittest individuals survive
2. **Crossover**: Combine traits from two parents
3. **Mutation**: Random changes introduce novelty
4. **Iteration**: Repeat for many generations

**GA applies this to optimization:**

1. **Population**: Collection of candidate solutions (e.g., 100 parameter sets)
2. **Fitness**: Evaluate each candidate (e.g., backtest Sharpe ratio)
3. **Selection**: Choose best candidates as parents
4. **Crossover**: Combine parents to create offspring
5. **Mutation**: Randomly perturb offspring
6. **Replacement**: New generation replaces old

**Repeat until convergence** (fitness plateaus or max generations reached).

### 7.3.3 Implementation

```lisp
;; Genetic algorithm for parameter optimization
;; Parameters:
;;   fitness-fn: function that evaluates a candidate (higher = better)
;;   param-ranges: array of {:min, :max} for each parameter
;;   pop-size: population size (e.g., 50-200)
;;   generations: number of generations to evolve
;; Returns: best individual found
(define (genetic-algorithm fitness-fn param-ranges pop-size generations)
  (let ((population (initialize-population param-ranges pop-size))
        (best-ever null)
        (best-fitness -999999))

    (for (gen (range 0 generations))
      ;; Evaluate fitness for all individuals
      (let ((fitness-scores
             (map population (lambda (individual) (fitness-fn individual)))))

        ;; Track best individual ever seen
        (let ((gen-best-idx (argmax fitness-scores))
              (gen-best-fitness (nth fitness-scores gen-best-idx)))

          (if (> gen-best-fitness best-fitness)
              (do
                (set! best-fitness gen-best-fitness)
                (set! best-ever (nth population gen-best-idx)))
              null)

          ;; Log progress every 10 generations
          (if (= (% gen 10) 0)
              (log :message "Generation" :gen gen
                   :best-fitness best-fitness)
              null))

        ;; Selection: tournament selection
        (let ((parents (tournament-selection population fitness-scores pop-size)))

          ;; Crossover and mutation: create next generation
          (let ((offspring (crossover-and-mutate parents param-ranges)))

            (set! population offspring)))))

    ;; Return best individual found
    best-ever))

;; Initialize random population
;; Each individual is a vector of parameter values
(define (initialize-population param-ranges pop-size)
  (let ((population []))

    (for (i (range 0 pop-size))
      (let ((individual
             (map param-ranges
                  (lambda (range)
                    ;; Random value in [min, max]
                    (+ (range :min)
                       (* (random) (- (range :max) (range :min))))))))

        (set! population (append population individual))))

    population))
```

### 7.3.4 Selection Methods

**Tournament Selection:** Randomly sample $k$ individuals, choose the best.

```lisp
;; Tournament selection
;; Randomly pick tournament-size individuals, select best
;; Repeat pop-size times to create parent pool
(define (tournament-selection population fitness-scores pop-size)
  (let ((parents [])
        (tournament-size 3))  ;; Typically 3-5

    (for (i (range 0 pop-size))
      ;; Random tournament
      (let ((tournament-indices
             (map (range 0 tournament-size)
                  (lambda (_) (floor (* (random) (length population)))))))

        ;; Find best in tournament
        (let ((best-idx
               (reduce tournament-indices
                       (nth tournament-indices 0)
                       (lambda (best idx)
                         (if (> (nth fitness-scores idx)
                               (nth fitness-scores best))
                             idx
                             best)))))

          (set! parents (append parents (nth population best-idx))))))

    parents))
```

**Roulette Wheel Selection:** Probability proportional to fitness.

```lisp
;; Roulette wheel selection (fitness-proportionate)
(define (roulette-selection population fitness-scores n)
  (let ((total-fitness (sum fitness-scores))
        (selected []))

    (for (i (range 0 n))
      (let ((spin (* (random) total-fitness))
            (cumulative 0)
            (selected-idx 0))

        ;; Find individual where cumulative fitness exceeds spin
        (for (j (range 0 (length fitness-scores)))
          (set! cumulative (+ cumulative (nth fitness-scores j)))

          (if (>= cumulative spin)
              (do
                (set! selected-idx j)
                (break))
              null))

        (set! selected (append selected (nth population selected-idx)))))

    selected))
```

### 7.3.5 Crossover and Mutation

**Uniform Crossover:** Each gene (parameter) has 50% chance from each parent.

**Mutation:** Small random perturbation with low probability.

```lisp
;; Crossover and mutation
(define (crossover-and-mutate parents param-ranges)
  (let ((offspring [])
        (crossover-rate 0.8)   ;; 80% of pairs undergo crossover
        (mutation-rate 0.1))   ;; 10% mutation probability per gene

    ;; Pair up parents (assumes even population size)
    (for (i (range 0 (/ (length parents) 2)))
      (let ((parent1 (nth parents (* i 2)))
            (parent2 (nth parents (+ (* i 2) 1))))

        ;; Initialize children as copies of parents
        (let ((child1 parent1)
              (child2 parent2))

          ;; Crossover (uniform)
          (if (< (random) crossover-rate)
              (do
                ;; Swap genes with 50% probability
                (set! child1 (map (range 0 (length parent1))
                                (lambda (j)
                                  (if (< (random) 0.5)
                                      (nth parent1 j)
                                      (nth parent2 j)))))

                (set! child2 (map (range 0 (length parent2))
                                (lambda (j)
                                  (if (< (random) 0.5)
                                      (nth parent2 j)
                                      (nth parent1 j))))))
              null)

          ;; Mutation
          (set! child1 (mutate-individual child1 param-ranges mutation-rate))
          (set! child2 (mutate-individual child2 param-ranges mutation-rate))

          (set! offspring (append offspring child1))
          (set! offspring (append offspring child2)))))

    offspring))

;; Mutate individual: randomly perturb genes
(define (mutate-individual individual param-ranges mutation-rate)
  (map (range 0 (length individual))
       (lambda (j)
         (if (< (random) mutation-rate)
             ;; Mutate: random value in parameter range
             (let ((range (nth param-ranges j)))
               (+ (range :min)
                  (* (random) (- (range :max) (range :min)))))

             ;; No mutation
             (nth individual j)))))
```

### 7.3.6 Example: SMA Crossover Strategy Optimization

**Problem:** Find optimal moving average periods for a crossover strategy.

**Parameters:**
- Fast MA period: 5-50 days
- Slow MA period: 20-200 days

**Fitness:** Backtest Sharpe ratio

```lisp
;; Fitness function: backtest SMA crossover strategy
;; params = [fast_period, slow_period]
(define (sma-strategy-fitness params prices)
  (let ((fast-period (floor (nth params 0)))   ;; Round to integer
        (slow-period (floor (nth params 1))))

    ;; Constraint: fast must be < slow
    (if (>= fast-period slow-period)
        -9999  ;; Invalid: return terrible fitness

        ;; Valid: backtest strategy
        (let ((strategy (sma-crossover-strategy fast-period slow-period)))
          (let ((backtest-result (backtest-strategy strategy prices 10000)))
            (backtest-result :sharpe-ratio))))))  ;; Fitness = Sharpe ratio

;; Run genetic algorithm
(define param-ranges
  [{:min 5 :max 50}      ;; Fast MA: 5-50 days
   {:min 20 :max 200}])  ;; Slow MA: 20-200 days

(define best-params
  (genetic-algorithm
    (lambda (params) (sma-strategy-fitness params historical-prices))
    param-ranges
    50        ;; Population size
    100))     ;; 100 generations

;; Result: e.g., [12, 45] (fast=12, slow=45)
;; These are the parameters with highest backtest Sharpe ratio
```

**GA vs Gradient Descent:**

| Aspect | Genetic Algorithm | Gradient Descent |
|--------|-------------------|------------------|
| Requires derivatives | ❌ No | ✅ Yes |
| Handles discrete params | ✅ Yes | ❌ No |
| Global optimum | Maybe (stochastic) | Only if convex |
| Computational cost | High (5000+ evaluations) | Low (100 evaluations) |
| Best for | Complex, black-box | Smooth, differentiable |

---

## 7.4 Simulated Annealing: Escaping Local Optima

### 7.4.1 The Metallurgy Analogy

**Annealing** is a metallurgical process:
1. Heat metal to high temperature (atoms move freely)
2. Slowly cool (atoms settle into low-energy state)
3. Result: Stronger, more stable structure

**Key insight:** At high temperature, atoms can escape local energy wells, avoiding suboptimal configurations.

**Simulated Annealing (SA)** applies this to optimization:
- "Temperature" = willingness to accept worse solutions
- High temperature = explore widely (accept many worse moves)
- Low temperature = exploit locally (accept few worse moves)
- **Gradual cooling** = transition from exploration to exploitation

### 7.4.2 The Algorithm

**Accept probability for worse solutions:**

$$P(\text{accept worse}) = \exp\left(-\frac{\Delta E}{T}\right)$$

where:
- $\Delta E$ = increase in objective (energy)
- $T$ = temperature (decreases over time)

**Intuition:**
- If $T$ is high, $e^{-\Delta E/T} \approx 1$ → accept almost anything
- If $T$ is low, $e^{-\Delta E/T} \approx 0$ → accept only improvements
- If $\Delta E$ is small, more likely to accept (small worsening)

```lisp
;; Simulated annealing
;; Parameters:
;;   objective: function to minimize (energy)
;;   initial-solution: starting point
;;   neighbor-fn: function that generates a neighbor solution
;;   initial-temp: starting temperature
;;   cooling-rate: multiply temperature by this each iteration (e.g., 0.95)
;;   max-iters: maximum iterations
(define (simulated-annealing objective initial-solution neighbor-fn
                            initial-temp cooling-rate max-iters)
  (let ((current-solution initial-solution)
        (current-energy (objective initial-solution))
        (best-solution initial-solution)
        (best-energy current-energy)
        (temperature initial-temp))

    (for (iter (range 0 max-iters))
      ;; Generate neighbor
      (let ((neighbor (neighbor-fn current-solution)))

        (let ((neighbor-energy (objective neighbor)))

          ;; Always accept better solutions
          (if (< neighbor-energy current-energy)
              (do
                (set! current-solution neighbor)
                (set! current-energy neighbor-energy)

                ;; Update best
                (if (< neighbor-energy best-energy)
                    (do
                      (set! best-solution neighbor)
                      (set! best-energy neighbor-energy))
                    null))

              ;; Accept worse solutions probabilistically
              (let ((delta-E (- neighbor-energy current-energy))
                    (acceptance-prob (exp (- (/ delta-E temperature)))))

                (if (< (random) acceptance-prob)
                    (do
                      (set! current-solution neighbor)
                      (set! current-energy neighbor-energy))
                    null)))))

      ;; Cool down: T ← T * cooling_rate
      (set! temperature (* temperature cooling-rate)))

    {:solution best-solution :energy best-energy}))
```

### 7.4.3 Neighbor Generation

**Key design choice:** How to generate neighbors?

For strategy parameters: Perturb by small random amount (±10% of range).

```lisp
;; Neighbor function: perturb one random parameter
(define (neighbor-solution-strategy params param-ranges)
  (let ((idx (floor (* (random) (length params))))  ;; Random parameter index
        (range (nth param-ranges idx)))

    ;; Perturbation: ±10% of parameter range
    (let ((perturbation (* 0.1 (- (range :max) (range :min))
                          (- (* 2 (random)) 1))))  ;; Uniform [-1, 1]

      (let ((new-val (+ (nth params idx) perturbation)))

        ;; Clamp to valid range
        (let ((clamped (max (range :min) (min (range :max) new-val))))

          ;; Create new parameter vector with mutated value
          (let ((new-params (copy-array params)))
            (set-nth! new-params idx clamped)
            new-params))))))

;; Example: Optimize SMA parameters with SA
(define sa-result
  (simulated-annealing
    (lambda (params) (- (sma-strategy-fitness params prices)))  ;; Minimize -Sharpe
    [10 30]                           ;; Initial guess
    (lambda (p) (neighbor-solution-strategy p param-ranges))
    100.0                             ;; Initial temperature
    0.95                              ;; Cooling rate (T ← 0.95*T)
    1000))                            ;; 1000 iterations

;; Result: {:solution [12, 45], :energy -1.8}
;; Sharpe ratio = 1.8 (minimized negative = maximized positive)
```

### 7.4.4 Cooling Schedules

The cooling schedule controls exploration vs exploitation trade-off:

| Schedule | Formula | Characteristics |
|----------|---------|----------------|
| Exponential | $T_k = T_0 \alpha^k$ | Fast, may miss global optimum |
| Linear | $T_k = T_0 - k \beta$ | Slow, thorough exploration |
| Logarithmic | $T_k = T_0 / \log(k+2)$ | Very slow, theoretical guarantee |
| Adaptive | Increase $T$ if stuck | Best empirical performance |

**Adaptive cooling:** Reheat if no improvement for many iterations.

```lisp
;; Adaptive simulated annealing (reheat if stuck)
(define (adaptive-annealing objective initial-solution neighbor-fn
                            initial-temp max-iters)
  (let ((temperature initial-temp)
        (no-improvement-count 0)
        (current-solution initial-solution)
        (current-energy (objective initial-solution))
        (best-solution initial-solution)
        (best-energy current-energy))

    (for (iter (range 0 max-iters))
      ;; (Standard SA accept/reject logic here)
      ;; ...

      ;; Track stagnation
      (if (= best-energy current-energy)
          (set! no-improvement-count (+ no-improvement-count 1))
          (set! no-improvement-count 0))

      ;; Adaptive cooling
      (if (> no-improvement-count 50)
          (do
            ;; Stuck: reheat to escape
            (set! temperature (* temperature 1.2))
            (set! no-improvement-count 0))

          ;; Normal cooling
          (set! temperature (* temperature 0.95))))

    {:solution best-solution :energy best-energy}))
```

---

## 7.5 Grid Search and Bayesian Optimization

### 7.5.1 Grid Search: Exhaustive Exploration

**Grid search:** Evaluate objective at every point on a grid.

**Example:** SMA parameters
- Fast: [5, 10, 15, 20, 25, ..., 50] (10 values)
- Slow: [20, 30, 40, ..., 200] (19 values)
- **Total evaluations:** 10 × 19 = 190

```lisp
;; Grid search for strategy parameters
(define (grid-search objective param-grids)
  (let ((best-params null)
        (best-score -9999))

    ;; Nested loops over parameter grids (2D example)
    (for (p1 (nth param-grids 0))
      (for (p2 (nth param-grids 1))

        (let ((params [p1 p2])
              (score (objective params)))

          (if (> score best-score)
              (do
                (set! best-score score)
                (set! best-params params))
              null))))

    {:params best-params :score best-score}))

;; Example: SMA crossover
(define grid-result
  (grid-search
    (lambda (p) (sma-strategy-fitness p prices))
    [(range 5 51 5)      ;; Fast: 5, 10, 15, ..., 50
     (range 20 201 10)]))  ;; Slow: 20, 30, 40, ..., 200

;; Evaluates 10 × 19 = 190 parameter combinations
```

**Curse of Dimensionality:**

Grid search scales **exponentially** with dimensions:

| Parameters | Values per Param | Total Evaluations |
|-----------|------------------|-------------------|
| 2 | 10 | 100 |
| 3 | 10 | 1,000 |
| 5 | 10 | 100,000 |
| 10 | 10 | 10,000,000,000 |

**Grid search is infeasible beyond 3-4 parameters.**

### 7.5.2 Random Search: Surprisingly Effective

**Random search:** Sample parameter values randomly.

**Bergstra & Bengio (2012):** "Random search is more efficient than grid search when only a few parameters matter."

**Intuition:** If only 2 out of 10 parameters affect the objective, random search explores those 2 dimensions thoroughly, while grid search wastes evaluations on irrelevant dimensions.

```lisp
;; Random search
(define (random-search objective param-ranges n-samples)
  (let ((best-params null)
        (best-score -9999))

    (for (i (range 0 n-samples))
      ;; Random sample from each parameter range
      (let ((params
             (map param-ranges
                  (lambda (range)
                    (+ (range :min)
                       (* (random) (- (range :max) (range :min))))))))

        (let ((score (objective params)))

          (if (> score best-score)
              (do
                (set! best-score score)
                (set! best-params params))
              null))))

    {:params best-params :score best-score}))

;; Example: 1000 random samples often beats 8000+ grid points
(define random-result
  (random-search
    (lambda (p) (sma-strategy-fitness p prices))
    param-ranges
    1000))
```

**Grid vs Random:**

- Grid search: 10×10 = 100 evaluations (fixed grid)
- Random search: 100 evaluations (random points)

If only 1 parameter matters:
- Grid: Explores 10 distinct values of important param
- Random: Explores ~100 distinct values of important param (much better!)

### 7.5.3 Bayesian Optimization: Smart Sampling

**Problem:** Random search wastes evaluations exploring bad regions.

**Bayesian Optimization:** Build a **probabilistic model** of the objective, use it to choose where to sample next.

**Process:**
1. Start with a few random samples
2. Fit a Gaussian Process (GP) to the observed points
3. Use **acquisition function** to choose next point (balance exploration/exploitation)
4. Evaluate objective at that point
5. Update GP, repeat

**Acquisition function (Upper Confidence Bound):**

$$\text{UCB}(x) = \mu(x) + \kappa \sigma(x)$$

where:
- $\mu(x)$ = GP mean prediction at $x$ (expected value)
- $\sigma(x)$ = GP standard deviation at $x$ (uncertainty)
- $\kappa$ = exploration parameter (typically 2)

**Intuition:** Choose points with high predicted value ($\mu$) OR high uncertainty ($\sigma$).

```lisp
;; Simplified Bayesian optimization (conceptual)
;; Real implementation requires GP library (scikit-optimize, GPyOpt)
(define (bayesian-optimization objective param-ranges n-iters)
  (let ((observations [])        ;; List of {:params, :score}
        (best-params null)
        (best-score -9999))

    ;; Phase 1: Random initialization (5 samples)
    (for (i (range 0 5))
      (let ((params (random-sample param-ranges)))
        (let ((score (objective params)))
          (set! observations (append observations {:params params :score score}))

          (if (> score best-score)
              (do
                (set! best-score score)
                (set! best-params params))
              null))))

    ;; Phase 2: Bayesian optimization (remaining iterations)
    (for (iter (range 0 (- n-iters 5)))
      ;; Fit GP to observations (conceptual—requires GP library)
      ;; gp = GaussianProcessRegressor.fit(X, y)

      ;; Choose next point via acquisition function (e.g., UCB)
      (let ((next-params (maximize-acquisition-function observations param-ranges)))

        (let ((score (objective next-params)))
          (set! observations (append observations
                                    {:params next-params :score score}))

          (if (> score best-score)
              (do
                (set! best-score score)
                (set! best-params next-params))
              null))))

    {:params best-params :score best-score}))

;; Acquisition: UCB (upper confidence bound)
;; In practice, use library to compute GP predictions
;; UCB(x) = μ(x) + κ*σ(x)  (high mean or high uncertainty)
```

**Efficiency Comparison:**

| Method | Evaluations | Global Optimum | Parallelizable |
|--------|------------|----------------|----------------|
| Grid Search | 10,000+ | No | Yes |
| Random Search | 1,000 | Unlikely | Yes |
| Genetic Algorithm | 5,000 | Maybe | Partially |
| Simulated Annealing | 2,000 | Maybe | No |
| **Bayesian Optimization** | **100-200** | **Likely** | **Limited** |

**Bayesian optimization is 10-50x more sample-efficient than random search.**

---

## 7.6 Constrained Optimization

### 7.6.1 Penalty Methods

**Problem:** Constraints complicate optimization.

**Example:** Portfolio weights must sum to 1 and be non-negative.

**Penalty method:** Convert constrained problem to unconstrained by adding penalties for constraint violations.

**Unconstrained form:**

$$\min_x f(x) + \mu \sum_i \max(0, g_i(x))^2$$

where:
- $f(x)$ = original objective
- $g_i(x) \leq 0$ = constraints
- $\mu$ = penalty coefficient (large $\mu$ → strong enforcement)

```lisp
;; Penalty method for portfolio optimization
(define (penalized-objective weights mu sigma risk-aversion penalty-coef)
  (let ((port-return (dot-product weights mu))
        (port-variance (quadratic-form weights sigma)))

    ;; Original objective: minimize risk - return
    (let ((objective (- port-variance (* risk-aversion port-return))))

      ;; Penalty 1: Weights must sum to 1
      (let ((sum-penalty (* penalty-coef (pow (- (sum weights) 1) 2)))

            ;; Penalty 2: Non-negative weights
            (neg-penalty (* penalty-coef
                           (sum (map weights
                                    (lambda (w)
                                      (pow (min 0 w) 2)))))))  ;; Penalty if w < 0

        (+ objective sum-penalty neg-penalty)))))

;; Optimize with increasing penalties
(define (optimize-portfolio-penalty mu sigma initial-weights)
  (let ((penalty-coef 1.0)
        (current-weights initial-weights))

    ;; Phase 1-5: Increase penalty gradually
    (for (phase (range 0 5))
      (set! penalty-coef (* penalty-coef 10))  ;; 1, 10, 100, 1000, 10000

      (let ((result
             (gradient-descent-nd
               (lambda (w) (penalized-objective w mu sigma 1.0 penalty-coef))
               (lambda (w) (numerical-gradient
                            (lambda (w2) (penalized-objective w2 mu sigma 1.0 penalty-coef))
                            w))
               current-weights
               0.01
               100
               0.001)))

        (set! current-weights (result :optimum))))

    current-weights))
```

### 7.6.2 Barrier Methods

**Barrier method:** Use logarithmic barriers to enforce constraints.

**Form:**

$$\min_x f(x) - \mu \sum_i \log(-g_i(x))$$

**Barrier prevents $g_i(x) \to 0^+$ (approaching constraint boundary from inside).**

```lisp
;; Log barrier for non-negativity
(define (barrier-objective weights mu sigma barrier-coef)
  (let ((port-return (dot-product weights mu))
        (port-variance (quadratic-form weights sigma)))

    (let ((sharpe (/ port-return (sqrt port-variance))))

      ;; Barrier for w > 0: -log(w)
      ;; As w → 0, -log(w) → ∞ (infinite penalty)
      (let ((barrier-penalty
             (- (* barrier-coef
                  (sum (map weights (lambda (w) (log w))))))))

        (+ (- sharpe) barrier-penalty)))))

;; Optimize with decreasing barrier coefficient
;; Start with large barrier, gradually reduce
```

### 7.6.3 Lagrange Multipliers: Analytical Solution

For equality constraints, **Lagrange multipliers** provide analytical solutions.

**Example: Minimum variance portfolio**

$$\begin{aligned}
\min_w \quad & w^T \Sigma w \\
\text{s.t.} \quad & \mathbf{1}^T w = 1
\end{aligned}$$

**Lagrangian:**

$$\mathcal{L}(w, \lambda) = w^T \Sigma w + \lambda(\mathbf{1}^T w - 1)$$

**Optimality conditions:**

$$\nabla_w \mathcal{L} = 2\Sigma w + \lambda \mathbf{1} = 0$$

**Solution:**

$$w^* = -\frac{\lambda}{2} \Sigma^{-1} \mathbf{1}$$

Apply constraint $\mathbf{1}^T w = 1$:

$$\lambda = -\frac{2}{\mathbf{1}^T \Sigma^{-1} \mathbf{1}}$$

**Final formula:**

$$w^* = \frac{\Sigma^{-1} \mathbf{1}}{\mathbf{1}^T \Sigma^{-1} \mathbf{1}}$$

```lisp
;; Minimum variance portfolio (analytical solution)
(define (minimum-variance-portfolio sigma)
  (let ((sigma-inv (matrix-inverse sigma))
        (ones (make-array (length sigma) 1)))

    (let ((sigma-inv-ones (matrix-vec-mult sigma-inv ones)))

      (let ((denom (dot-product ones sigma-inv-ones)))

        ;; w* = Σ⁻¹ 1 / (1^T Σ⁻¹ 1)
        (map sigma-inv-ones (lambda (x) (/ x denom)))))))

;; Example
(define min-var-w (minimum-variance-portfolio sigma))
;; Minimizes portfolio variance subject to full investment
```

---

## 7.7 Practical Applications

### 7.7.1 Walk-Forward Optimization

**Problem:** In-sample optimization overfits.

**Solution: Walk-forward analysis** simulates realistic deployment:

1. **Train:** Optimize parameters on historical window (e.g., 1 year)
2. **Test:** Apply optimized parameters to out-of-sample period (e.g., 1 month)
3. **Roll forward:** Shift window, repeat

```lisp
;; Walk-forward optimization
;; Parameters:
;;   prices: full price history
;;   train-period: number of periods for training
;;   test-period: number of periods for testing
;;   param-ranges: parameter search space
(define (walk-forward-optimize prices train-period test-period param-ranges)
  (let ((n (length prices))
        (results []))

    ;; Rolling window: train, then test
    (for (i (range train-period (- n test-period) test-period))

      ;; Train on [i - train_period, i]
      (let ((train-prices (slice prices (- i train-period) i)))

        (let ((optimal-params
               (genetic-algorithm
                 (lambda (p) (sma-strategy-fitness p train-prices))
                 param-ranges
                 30    ;; Small population (fast)
                 50)))  ;; Few generations

          ;; Test on [i, i + test_period]
          (let ((test-prices (slice prices i (+ i test-period))))

            (let ((test-sharpe (sma-strategy-fitness optimal-params test-prices)))

              (set! results (append results
                                   {:train-end i
                                    :params optimal-params
                                    :test-sharpe test-sharpe})))))))

    results))

;; Aggregate out-of-sample performance
(define (walk-forward-summary results)
  {:avg-sharpe (average (map results (lambda (r) (r :test-sharpe))))
   :periods (length results)})

;; Example
(define wf-results
  (walk-forward-optimize historical-prices 252 21 param-ranges))

(define wf-summary (walk-forward-summary wf-results))
;; avg-sharpe: realistic estimate (not overfitted)
```

**Walk-Forward vs In-Sample:**

| Metric | In-Sample Optimized | Walk-Forward |
|--------|-------------------|--------------|
| Sharpe Ratio | 2.5 (optimistic) | 1.2 (realistic) |
| Max Drawdown | 15% | 28% |
| Win Rate | 65% | 52% |

**In-sample results are always better—that's overfitting!**

### 7.7.2 Kelly Criterion: Optimal Position Sizing

**Kelly Criterion:** Maximize geometric growth rate by betting a fraction of capital proportional to edge.

**Formula:**

$$f^* = \frac{p \cdot b - q}{b}$$

where:
- $p$ = win probability
- $q = 1 - p$ = loss probability
- $b$ = win/loss ratio (payoff when win / loss when lose)
- $f^*$ = fraction of capital to risk

```lisp
;; Kelly fraction calculation
(define (kelly-fraction win-prob win-loss-ratio)
  (let ((lose-prob (- 1 win-prob)))
    (/ (- (* win-prob win-loss-ratio) lose-prob)
       win-loss-ratio)))

;; Example: 55% win rate, 2:1 win/loss ratio
(define kelly-f (kelly-fraction 0.55 2.0))
;; → 0.325 (risk 32.5% of capital per trade)

;; Fractional Kelly (reduce risk)
(define (fractional-kelly win-prob win-loss-ratio fraction)
  (* fraction (kelly-fraction win-prob win-loss-ratio)))

;; Half-Kelly: more conservative
(define half-kelly (fractional-kelly 0.55 2.0 0.5))
;; → 0.1625 (risk 16.25%)
```

**Why fractional Kelly?**

Full Kelly maximizes growth but has high volatility. Half-Kelly sacrifices some growth for much lower drawdowns.

### 7.7.3 Transaction Cost Optimization

**Problem:** Optimal theoretical portfolio may be unprofitable after transaction costs.

**Solution:** Explicitly model costs in objective.

```lisp
;; Portfolio optimization with transaction costs
(define (portfolio-with-costs mu sigma current-weights transaction-cost-rate)
  ;; Optimal weights (ignoring costs)
  (let ((optimal-weights (markowitz-portfolio mu sigma 2.0)))

    ;; Calculate turnover (sum of absolute changes)
    (let ((turnover
           (sum (map (range 0 (length current-weights))
                    (lambda (i)
                      (abs (- (nth optimal-weights i)
                             (nth current-weights i))))))))

      ;; Expected return after costs
      (let ((gross-return (dot-product optimal-weights mu))
            (costs (* transaction-cost-rate turnover)))

        ;; Only rebalance if net return > current return
        (if (> (- gross-return costs)
              (dot-product current-weights mu))
            optimal-weights  ;; Rebalance
            current-weights))))) ;; Don't trade

;; Example
(define current-w [0.3 0.4 0.3])
(define new-w (portfolio-with-costs mu sigma current-w 0.001))
;; If turnover*0.1% < gain, rebalance; else hold
```

---

## 7.8 Key Takeaways

**Algorithm Selection Framework:**

| Problem Type | Recommended Method | Rationale |
|-------------|-------------------|-----------|
| Smooth, differentiable | Adam optimizer | Fast, robust |
| Convex (portfolio) | QP solver (CVXPY) | Guaranteed global optimum |
| Discrete parameters | Genetic algorithm | Handles integers naturally |
| Expensive objective | Bayesian optimization | Sample-efficient (100 evals) |
| Multi-objective | NSGA-II | Finds Pareto frontier |
| Non-smooth | Simulated annealing | Escapes local minima |

**Common Pitfalls:**

- ⚠️ **Overfitting:** In-sample optimization ≠ future performance → use walk-forward
- ⚠️ **Ignoring costs:** Theoretical optimum may be unprofitable after fees
- ⚠️ **Parameter instability:** Optimal parameters change over time → re-optimize periodically
- ⚠️ **Curse of dimensionality:** Grid search fails beyond 3 parameters → use Bayesian optimization
- ⚠️ **Local optima:** Gradient descent gets stuck → use multiple random starts or GA

**Performance Benchmarks (2-parameter problem):**

| Method | Time | Evaluations |
|--------|------|-------------|
| Grid Search | 1 min | 10,000 |
| Random Search | 5 sec | 1,000 |
| Genetic Algorithm | 30 sec | 5,000 |
| Simulated Annealing | 20 sec | 2,000 |
| Bayesian Optimization | 20 sec | 100 |
| **Gradient Descent** | **1 sec** | **50** |

**For 10-parameter problem:**

| Method | Time |
|--------|------|
| Grid Search | 10 hours (10^10 evals) |
| Random Search | 50 sec |
| Genetic Algorithm | 5 min |
| Bayesian Optimization | 3 min |

**Next Steps:**

Chapter 8 (Risk Management) uses these optimization techniques to:
- Optimize position sizes (Kelly criterion)
- Minimize portfolio drawdown (constrained optimization)
- Calibrate risk models (maximum likelihood estimation)

The optimization skills you've learned here are foundational—every quantitative trading decision involves optimization.

---

## Further Reading

1. **Nocedal, J., & Wright, S. (2006)**. *Numerical Optimization* (2nd ed.). Springer.
   - The definitive reference for gradient-based methods—comprehensive and rigorous.

2. **Boyd, S., & Vandenberghe, L. (2004)**. *Convex Optimization*. Cambridge University Press.
   - Free online: https://web.stanford.edu/~boyd/cvxbook/
   - Beautiful treatment of convex optimization with applications.

3. **Deb, K. (2001)**. *Multi-Objective Optimization using Evolutionary Algorithms*. Wiley.
   - Deep dive into genetic algorithms and multi-objective optimization.

4. **Bergstra, J., & Bengio, Y. (2012)**. "Random Search for Hyper-Parameter Optimization". *Journal of Machine Learning Research*, 13, 281-305.
   - Empirical evidence that random search beats grid search.

5. **Pardo, R. (2008)**. *The Evaluation and Optimization of Trading Strategies* (2nd ed.). Wiley.
   - Practical guide to walk-forward optimization and robustness testing.

6. **Mockus, J. (2012)**. *Bayesian Approach to Global Optimization*. Springer.
   - Theoretical foundations of Bayesian optimization.

---

**Navigation:**
- [← Chapter 6: Stochastic Processes](06_stochastic_processes.md)
- [→ Chapter 8: Risk Management](08_risk_management.md) (when available)
