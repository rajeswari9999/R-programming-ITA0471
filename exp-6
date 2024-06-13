get_primes_up_to <- function(n) {
  is_prime <- rep(TRUE, n + 1)
  is_prime[1] <- FALSE
  primes <- c()

  for (i in 2:n) {
    if (is_prime[i]) {
      primes <- c(primes, i)
      for (j in (i * i):n) {
        if (j %% i == 0) {
          is_prime[j] <- FALSE
        }
      }
    }
  }
  
  return(primes)
}
upper_limit <- 50
prime_numbers <- get_primes_up_to(upper_limit)
print(prime_numbers)
