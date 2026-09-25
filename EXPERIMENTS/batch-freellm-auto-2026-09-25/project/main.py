from math_utils import factorial, is_prime, fibonacci

def demo():
    n_factorial = 5
    n_fibo = 10
    prime_candidate = 29

    print(f"Factorial of {n_factorial} is {factorial(n_factorial)}")
    print(f"Is {prime_candidate} a prime? {'Yes' if is_prime(prime_candidate) else 'No'}")
    print(f"The {n_fibo}th Fibonacci number is {fibonacci(n_fibo)}")

if __name__ == "__main__":
    demo()
