;; short-winning-rfp.clar
;; A brief unique RFP contract for Google Clarity Web3

(clarity-version 2)

;; Errors
(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-BAD-TIME (err u101))
(define-constant ERR-HASH-MISMATCH (err u102))

;; Counters
(define-data-var next-id uint u0)

;; RFP data
(define-map rfps ((id uint))
  ((owner principal) (title (string-ascii 64))
   (commit-dead uint) (reveal-dead uint) (winner (optional principal))))

;; Commits: (id, vendor) -> hash
(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))

;; Reveals: (id, vendor) -> proposal
(define-map reveals ((id uint) (vendor principal)) ((uri (string-utf8 120))))

;; Create new RFP
(define-public (create-rfp (title (string-ascii 64)) (commit-dead uint) (reveal-dead uint))
  (begin
    (asserts! (> commit-dead block-height) ERR-BAD-TIME)
    (asserts! (> reveal-dead commit-dead) ERR-BAD-TIME)
    (let ((id (+ (var-get next-id) u1)))
      (map-set rfps { id: id }
        { owner: tx-sender, title: title,
          commit-dead: commit-dead, reveal-dead: reveal-dead, winner: none })
      (var-set next-id id)
      (ok id))))

;; Vendor commit: sha256(id || vendor || uri || salt)
(define-public (commit (id uint) (h (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (begin
             (asserts! (<= block-height (get commit-dead (unwrap! r ERR-NOT-FOUND))) ERR-BAD-TIME)
             (map-set commits { id: id, vendor: tx-sender } { h: h })
             (ok true))
      none ERR-NOT-FOUND)))

;; Vendor reveal
(define-public (reveal (id uint) (uri (string-utf8 120)) (salt (buff 32)))
  (let ((c (map-get? commits { id: id, vendor: tx-sender })))
    (match c
      somec (let ((stored (get h (unwrap! c ERR-NOT-FOUND)))
                  (calc (sha256 (concat (to-buff id) (concat (to-buff (hash160 tx-sender)) (concat (utf8-to-bytes uri) salt)))))))
              (asserts! (is-eq stored calc) ERR-HASH-MISMATCH)
              (map-set reveals { id: id, vendor: tx-sender } { uri: uri })
              (ok true))
      none ERR-NOT-FOUND)))

;; Finalize – owner picks winner among revealed vendors
(define-public (finalize (id uint) (winner principal))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! r ERR-NOT-FOUND)))
             (asserts! (is-eq (get owner rfp) tx-sender) ERR-NOT-FOUND)
             (asserts! (> block-height (get reveal-dead rfp)) ERR-BAD-TIME)
             (match (map-get? reveals { id: id, vendor: winner })
               somev (begin
                        (map-set rfps { id: id } (merge rfp { winner: (some winner) }))
                        (ok winner))
               none ERR-NOT-FOUND))
      none ERR-NOT-FOUND)))
