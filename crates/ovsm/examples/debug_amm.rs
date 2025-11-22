use ovsm::compiler::debug_compile;

fn main() {
    let amm = r#"
;; AMM Constant Product Swap
;; Load pool reserves from account 0
(define pool_account (get accounts 0))
(define reserves_x (mem-load pool_account 0))
(define reserves_y (mem-load pool_account 8))

;; Load swap amount from instruction data
(define swap_amount (mem-load instruction-data 0))

;; Constants
(define FEE_BPS 30)
(define BPS_DENOMINATOR 10000)

;; Calculate fee
(define fee (/ (* swap_amount FEE_BPS) BPS_DENOMINATOR))
(define amount_in_after_fee (- swap_amount fee))

;; Constant product formula: dy = y * dx / (x + dx)
(define new_reserves_x (+ reserves_x amount_in_after_fee))
(define amount_out (/ (* reserves_y amount_in_after_fee) new_reserves_x))

;; Update pool state
(mem-store pool_account 0 new_reserves_x)
(mem-store pool_account 8 (- reserves_y amount_out))

;; Return output amount
amount_out
"#;

    debug_compile(amm);
}
