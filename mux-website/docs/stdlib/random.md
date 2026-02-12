# Random Module

The `random` module provides pseudorandom number generation for Mux programs. It uses a time-based seed by default, but can be manually seeded for reproducible results.

## Import

```mux
import random
```

## Functions

### random.seed

```mux
random.seed(int seed) returns void
```

Initialize the random number generator with a specific seed. Use this when you need reproducible random sequences, such as in testing or simulations.

```mux title="seed_example.mux"
import random

func main() returns void {
    // Seed for reproducible results
    random.seed(12345)
    
    auto a = random.next_int()
    
    // Same seed produces same sequence
    random.seed(12345)
    auto b = random.next_int()
    
    // a and b will be equal
    print(a.to_string())
}
```

### random.next_int

```mux
random.next_int() returns int
```

Generate a random integer between 0 and RAND_MAX (platform-dependent, typically 2,147,483,647).

The generator auto-initializes with the current time on first use if not explicitly seeded.

```mux title="dice_roll.mux"
import random

func roll_die() returns int {
    // Random number from 1 to 6
    return random.next_range(1, 7)
}

func main() returns void {
    auto roll = roll_die()
    print("You rolled a " + roll.to_string())
}
```

### random.next_range

```mux
random.next_range(int min, int max) returns int
```

Generate a random integer in the range [min, max). The lower bound is inclusive, the upper bound is exclusive.

Returns `min` if `min >= max`.

```mux title="lottery_numbers.mux"
import random

func generate_lotto_numbers() returns list<int> {
    list<int> numbers = []
    
    for _ in range(0, 6) {
        // Numbers from 1 to 49
        auto num = random.next_range(1, 50)
        numbers.push_back(num)
    }
    
    return numbers
}

func main() returns void {
    auto lotto = generate_lotto_numbers()
    print("Your lottery numbers:")
    for num in lotto {
        print(num.to_string())
    }
}
```

### random.next_float

```mux
random.next_float() returns float
```

Generate a random floating-point number in the range [0.0, 1.0).

Useful for probabilities, animations, and scientific calculations.

```mux title="probability_example.mux"
import random

func should_event_occur(float probability) returns bool {
    auto roll = random.next_float()
    return roll < probability
}

func main() returns void {
    // 30% chance of rare event
    if should_event_occur(0.3) {
        print("Rare event occurred!")
    } else {
        print("Nothing special happened.")
    }
}
```

### random.next_bool

```mux
random.next_bool() returns bool
```

Generate a random boolean value (true or false) with equal probability (50/50).

```mux title="coin_flip.mux"
import random

func flip_coin() returns string {
    if random.next_bool() {
        return "Heads"
    }
    return "Tails"
}

func main() returns void {
    auto result = flip_coin()
    print("Coin flip result: " + result)
}
```

## Complete Example

```mux title="random_demo.mux"
import random

func main() returns void {
    // Seed for reproducible output
    random.seed(42)
    
    print("=== Random Number Demo ===")
    
    // Generate some random integers
    print("Random integers:")
    for i in range(0, 5) {
        auto num = random.next_int()
        print(num.to_string())
    }
    
    // Generate random numbers in range
    print("\nRandom numbers (1-100):")
    for i in range(0, 5) {
        auto num = random.next_range(1, 101)
        print(num.to_string())
    }
    
    // Generate random floats
    print("\nRandom floats:")
    for i in range(0, 3) {
        auto f = random.next_float()
        print(f.to_string())
    }
    
    // Generate random booleans
    print("\nRandom booleans:")
    for i in range(0, 5) {
        auto b = random.next_bool()
        print(b.to_string())
    }
}
```

## Implementation Details

The random module uses the standard C library's `rand()` function with the following characteristics:

- **Thread safety**: Uses atomic operations for initialization state
- **Auto-initialization**: Seeds automatically with current time if not explicitly seeded
- **Range distribution**: Uses modulo arithmetic for `next_range`, which may have slight bias for very large ranges
- **Float precision**: Produces values with approximately 15 bits of precision

For cryptographic applications or high-precision simulations, consider using a specialized library.
