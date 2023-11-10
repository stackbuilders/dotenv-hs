# JWT Encoding with Haskell

This Haskell program demonstrates how to encode a JSON Web Token (JWT) using the HMAC-SHA256 algorithm. The code uses the `Jose` library for JWT handling and `dotenv-hs` for loading environment variables.

## Prerequisites

Before running the program, make sure you have the necessary dependencies installed:

- [Jose](https://hackage.haskell.org/package/jose)
- [Configuration.Dotenv](https://hackage.haskell.org/package/dotenv)

## Usage

1. Clone the repository:

```bash
git clone https://github.com/stackbuilders/dotenv-hs
```

2. Navigate to the project directory:

```bash
cd your-repo
```

3. Create a `.env` file in the project root and set your JWT secret:

```env
SECRET=your_secret_key
```

4. Build and run the Haskell program:

```bash
ghc your-program.hs
./your-program
```

## Explanation

The program follows these steps:

1. Loads configuration from the default configuration file using `Configuration.Dotenv`.
2. Retrieves the JWT secret from the `SECRET` environment variable.
3. Encodes a sample JSON message using HMAC-SHA256 with the obtained secret.
4. Prints the encoded JWT.

Note: Ensure that you have appropriate error handling and security measures in a production environment.

Feel free to modify the code and adapt it to your specific use case.
