#!/bin/bash

# File 66: Credit Spread Strategies
cat > /home/larp/larpdevs/osvm-cli/examples/ovsm_complete/66_credit_spread_strategies.ovsm << 'EOF'
;; ============================================================================
;; OVSM LISP Example 66: CREDIT SPREAD STRATEGIES
;; ============================================================================
;;
;; THEORY: Credit Spread Trading
;; ============================================================================
;;
;; Credit spread strategies involve trading the differential in yields between
;; bonds with different credit qualities. These spreads compensate investors
;; for taking on default risk and reflect market perceptions of credit quality.
;;
;; 1. CREDIT SPREAD COMPONENTS:
;;    - Default Risk Premium: Compensation for probability of default
;;    - Liquidity Premium: Compensation for trading costs
;;    - Systematic Risk: Correlation with market factors
;;    - Recovery Risk: Uncertainty in recovery value
;;
;; 2. SPREAD PRODUCTS:
;;    - Corporate Bonds: Investment grade vs high yield
;;    - Credit Default Swaps: Pure credit exposure
;;    - Loans: Bank loans and leveraged loans
;;    - Convertible Bonds: Hybrid equity/credit
;;
;; 3. TRADING STRATEGIES:
;;    - Long/Short Credit: Relative value between issuers
;;    - Capital Structure Arbitrage: Senior vs subordinated debt
;;    - Curve Trading: Short-dated vs long-dated credit
;;    - Index vs Singles: ETF arbitrage
;;
;; 4. CREDIT SPREAD DRIVERS:
;;    - Default Probability: Fundamental credit quality
;;    - Economic Cycle: Expansion vs recession
;;    - Risk Appetite: Risk-on vs risk-off sentiment
;;    - Supply/Demand: Issuance and fund flows
;;
;; 5. KEY METRICS:
;;    - Credit Spread: Yield over benchmark (Treasuries or swaps)
;;    - Credit DV01: Dollar sensitivity to 1bp spread change
;;    - Spread Duration: Time for spread mean reversion
;;    - Transition Matrix: Probability of rating changes
;;
;; Mathematical Framework:
;;
;; Credit Spread (Risk-Neutral):
;; s = (PD × LGD) / (1 - PD × LGD)
;; Where:
;; - s = credit spread
;; - PD = probability of default
;; - LGD = loss given default (1 - recovery rate)
;;
;; Merton Model (Structural):
;; PD = N(-d2)
;; d2 = (ln(V/D) + (r - 0.5σ²)T) / (σ√T)
;; Where:
;; - V = firm value
;; - D = debt value
;; - σ = firm value volatility
;; - T = time horizon
;;
;; Expected Loss:
;; EL = PD × LGD × Exposure
;;
;; Credit VaR:
;; CVaR = Σ(w_i × Exposure_i × σ_spread_i × z_α)
;; Where w_i = portfolio weight, z_α = confidence level
;;
;; ============================================================================

(do
  ;; Market data
  (define credit-universe [
    {:issuer "AAA Corp" :rating "AAA" :spread 0.35 :dv01 8.5 :maturity 10 :sector "tech"}
    {:issuer "BBB Corp" :rating "BBB" :spread 1.25 :dv01 8.2 :maturity 10 :sector "tech"}
    {:issuer "BB Corp" :rating "BB" :spread 3.50 :dv01 7.8 :maturity 10 :sector "tech"}
    {:issuer "AA Util" :rating "AA" :spread 0.65 :dv01 8.4 :maturity 10 :sector "utility"}
    {:issuer "A Bank" :rating "A" :spread 0.95 :dv01 8.3 :maturity 10 :sector "financial"}
  ])

  (define transition-matrix {:AAA {:AAA 0.95 :AA 0.04 :A 0.01 :BBB 0 :default 0}
                             :AA {:AAA 0.02 :AA 0.93 :A 0.04 :BBB 0.01 :default 0}
                             :A {:AA 0.01 :A 0.91 :BBB 0.07 :BB 0.01 :default 0}
                             :BBB {:A 0.01 :BBB 0.88 :BB 0.08 :B 0.02 :default 0.01}
                             :BB {:BBB 0.02 :BB 0.85 :B 0.10 :CCC 0.02 :default 0.01}})

  (define macro-indicators {:gdp-growth 2.5 :unemployment 4.2 :credit-cycle "mid"
                           :fed-policy "neutral" :vix 18.5})

  (log :message "=== CREDIT SPREAD STRATEGY ANALYSIS ===" :value "")

  ;; ========================================================================
  ;; PART 1: SPREAD LEVEL ANALYSIS
  ;; ========================================================================

  (log :message "\n--- Current Spread Levels ---" :value "")

  (for (bond credit-universe)
    (log :message (bond :issuer) :value (bond :spread)))

  ;; Calculate spread differentials
  (define aaa-bond (nth credit-universe 0))
  (define bbb-bond (nth credit-universe 1))
  (define bb-bond (nth credit-universe 2))

  (define ig-spread-differential (- (bbb-bond :spread) (aaa-bond :spread)))
  (define hy-spread-differential (- (bb-bond :spread) (bbb-bond :spread)))
  (define aaa-hy-differential (- (bb-bond :spread) (aaa-bond :spread)))

  (log :message "\nSpread Differentials:" :value "")
  (log :message "  IG Differential (BBB-AAA):" :value ig-spread-differential)
  (log :message "  HY Differential (BB-BBB):" :value hy-spread-differential)
  (log :message "  AAA-HY Differential:" :value aaa-hy-differential)

  ;; Historical percentile analysis
  (define ig-hist-mean 0.85)
  (define ig-hist-std 0.25)
  (define ig-zscore (/ (- ig-spread-differential ig-hist-mean) ig-hist-std))

  (log :message "\nIG Spread Historical Context:" :value "")
  (log :message "  Current:" :value ig-spread-differential)
  (log :message "  Historical Mean:" :value ig-hist-mean)
  (log :message "  Z-Score:" :value ig-zscore)

  ;; ========================================================================
  ;; PART 2: DEFAULT PROBABILITY ESTIMATION
  ;; ========================================================================

  (log :message "\n--- Default Probability Analysis ---" :value "")

  ;; Estimate PD from credit spread (simplified)
  (define recovery-rate 0.40)
  (define lgd (- 1 recovery-rate))

  (for (bond credit-universe)
    (do
      (define spread (/ (bond :spread) 100))
      (define implied-pd (/ spread (+ lgd (- 1 (* spread lgd)))))
      (log :message (bond :issuer) :value (* implied-pd 100))))

  ;; Merton model estimation for BBB Corp
  (define firm-value 1000)
  (define debt-value 600)
  (define firm-volatility 0.25)
  (define time-horizon 10)
  (define risk-free 0.05)

  (define leverage-ratio (/ debt-value firm-value))
  (define distance-to-default 
    (/ (+ (- 1 leverage-ratio) (* risk-free time-horizon))
       (* firm-volatility 1.414)))

  (log :message "\nMerton Model (BBB Corp):" :value "")
  (log :message "  Leverage Ratio:" :value leverage-ratio)
  (log :message "  Distance to Default:" :value distance-to-default)

  ;; ========================================================================
  ;; PART 3: LONG/SHORT CREDIT STRATEGY
  ;; ========================================================================

  (log :message "\n--- Long/Short Credit Pair Trade ---" :value "")

  ;; Strategy: Long BBB Corp (cheap) vs Short AAA Corp (rich)
  (define notional 10000000)
  (define long-bond bbb-bond)
  (define short-bond aaa-bond)

  ;; Duration-neutral weights
  (define long-weight notional)
  (define short-weight (* notional (/ (long-bond :dv01) (short-bond :dv01))))

  (log :message "Position Sizing:" :value "")
  (log :message "  Long Position:" :value long-weight)
  (log :message "  Short Position:" :value short-weight)

  ;; Calculate net spread
  (define net-spread (- (long-bond :spread) (short-bond :spread)))
  (log :message "Net Spread Capture:" :value net-spread)

  ;; P&L scenarios
  (define scenario-convergence-10bp 
    (* 10 (+ (* (long-bond :dv01) long-weight)
             (* (short-bond :dv01) short-weight -1))))

  (log :message "\nScenario: 10bp Spread Convergence" :value scenario-convergence-10bp)

  ;; ========================================================================
  ;; PART 4: CAPITAL STRUCTURE ARBITRAGE
  ;; ========================================================================

  (log :message "\n--- Capital Structure Arbitrage ---" :value "")

  (define senior-bond {:seniority "senior" :spread 1.20 :recovery 0.60})
  (define subordinated-bond {:seniority "subordinated" :spread 2.40 :recovery 0.30})

  (define seniority-premium (- (subordinated-bond :spread) (senior-bond :spread)))
  (log :message "Seniority Premium:" :value seniority-premium)

  ;; Fair premium based on recovery differential
  (define recovery-diff (- (senior-bond :recovery) (subordinated-bond :recovery)))
  (define fair-premium (* recovery-diff 4.0))

  (log :message "Fair Seniority Premium:" :value fair-premium)
  (log :message "Premium Richness:" :value (- seniority-premium fair-premium))

  ;; ========================================================================
  ;; PART 5: TRANSITION RISK ANALYSIS
  ;; ========================================================================

  (log :message "\n--- Credit Migration Analysis ---" :value "")

  ;; Calculate expected spread change from rating transitions
  (define current-rating "BBB")
  (define transitions (transition-matrix :BBB))

  (define spread-map {:AAA 0.35 :AA 0.50 :A 0.75 :BBB 1.25 :BB 3.50 :B 6.00 :CCC 12.00})

  (define expected-spread 0)
  (set! expected-spread (+ expected-spread (* (transitions :A) (spread-map :A))))
  (set! expected-spread (+ expected-spread (* (transitions :BBB) (spread-map :BBB))))
  (set! expected-spread (+ expected-spread (* (transitions :BB) (spread-map :BB))))
  (set! expected-spread (+ expected-spread (* (transitions :B) (spread-map :B))))

  (log :message "Current Spread (BBB):" :value (spread-map :BBB))
  (log :message "Expected Spread (1Y):" :value expected-spread)
  (log :message "Migration Risk:" :value (- expected-spread (spread-map :BBB)))

  ;; ========================================================================
  ;; PART 6: MACRO REGIME ANALYSIS
  ;; ========================================================================

  (log :message "\n--- Macro Credit Regime ---" :value "")

  (define regime-score 0)
  
  ;; GDP growth factor
  (if (> (macro-indicators :gdp-growth) 2.0)
      (set! regime-score (+ regime-score 1))
      (set! regime-score (- regime-score 1)))

  ;; Unemployment factor
  (if (< (macro-indicators :unemployment) 5.0)
      (set! regime-score (+ regime-score 1))
      (set! regime-score (- regime-score 1)))

  ;; VIX factor
  (if (< (macro-indicators :vix) 20)
      (set! regime-score (+ regime-score 1))
      (set! regime-score (- regime-score 1)))

  (log :message "Regime Score:" :value regime-score)

  (define regime-signal
    (if (> regime-score 1)
        "RISK-ON: Favor credit exposure"
        (if (< regime-score -1)
            "RISK-OFF: Reduce credit exposure"
            "NEUTRAL: Maintain positions")))

  (log :message "Regime Signal:" :value regime-signal)

  ;; ========================================================================
  ;; PART 7: PORTFOLIO CONSTRUCTION
  ;; ========================================================================

  (log :message "\n--- Credit Portfolio Construction ---" :value "")

  (define portfolio-notional 50000000)
  (define target-spread 2.0)

  ;; Allocate to achieve target spread
  (define ig-allocation 0.60)
  (define hy-allocation 0.40)

  (define portfolio-spread 
    (+ (* ig-allocation (bbb-bond :spread))
       (* hy-allocation (bb-bond :spread))))

  (log :message "Target Spread:" :value target-spread)
  (log :message "Portfolio Spread:" :value portfolio-spread)
  (log :message "IG Allocation:" :value (* ig-allocation 100))
  (log :message "HY Allocation:" :value (* hy-allocation 100))

  ;; Calculate portfolio metrics
  (define portfolio-dv01
    (+ (* ig-allocation portfolio-notional (bbb-bond :dv01))
       (* hy-allocation portfolio-notional (bb-bond :dv01))))

  (log :message "Portfolio DV01:" :value portfolio-dv01)

  ;; Risk metrics
  (define spread-volatility 0.20)
  (define daily-var (* portfolio-dv01 spread-volatility 2.33))

  (log :message "Daily 99% VaR:" :value daily-var)

  ;; ========================================================================
  ;; SUMMARY
  ;; ========================================================================

  (log :message "\n=== CREDIT SPREAD STRATEGY SUMMARY ===" :value "")

  (define strategy-recommendation
    (if (< ig-zscore -1.5)
        "BUY IG CREDIT: Spreads wide vs history"
        (if (> ig-zscore 1.5)
            "SELL IG CREDIT: Spreads tight vs history"
            (if (> regime-score 1)
                "NEUTRAL/LONG BIAS: Macro supportive"
                "NEUTRAL/SHORT BIAS: Macro headwinds"))))

  (log :message "Primary Strategy:" :value strategy-recommendation)
  (log :message "IG Z-Score:" :value ig-zscore)
  (log :message "Macro Regime:" :value regime-signal)
  (log :message "Expected Spread Change:" :value (- expected-spread (bbb-bond :spread)))

  (log :message "\n=== ANALYSIS COMPLETE ===" :value (now)))
EOF

echo "Created 66_credit_spread_strategies.ovsm"
