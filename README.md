# llm-hs

A CLI tool for calling various LLM APIs (OpenAI, Claude, Ollama, Gemini) from the command line, written in Haskell.

## Features

- Support for multiple LLM providers:
  - OpenAI (GPT-4, GPT-4o-mini, etc.) - **Streaming ✅**
  - Anthropic Claude (Claude 3.5 Sonnet, etc.) - **Streaming ✅**
  - Ollama (local LLM server) - **Streaming ✅**
  - Google Gemini (Gemini 1.5 Flash, etc.) - **Streaming ✅**
- **Real-time streaming output** for all providers
- **Interactive mode** for multi-turn conversations (REPL)
- Pipe mode for single-shot queries
- Automatic conversation history management in interactive mode
- Configurable via command-line arguments or environment variables

## Installation

```bash
stack build
stack install
```

## Usage

### Basic Usage

The tool supports two modes:

#### 1. Pipe Mode (Single Query)

When you provide input via stdin, the tool processes a single query and exits:

```bash
# Using OpenAI
echo "Hello, how are you?" | llm-hs-exe --provider openai

# Using Claude
echo "Explain quantum computing" | llm-hs-exe --provider claude

# Using Ollama (local)
echo "Write a haiku" | llm-hs-exe --provider ollama

# Using Gemini
echo "What is Haskell?" | llm-hs-exe --provider gemini
```

#### 2. Interactive Mode (Multi-turn Conversation)

When you run the tool without piping input, it starts an interactive REPL session with **Emacs keybindings** support:

```bash
# Start interactive session with OpenAI
llm-hs-exe --provider openai

# Start interactive session with streaming
llm-hs-exe --provider claude --stream

# Example session:
# === Interactive Mode ===
# Provider: OpenAI
# Type your message and press Enter. Type 'exit' or 'quit' to end.
# Emacs keybindings supported (C-a, C-e, C-k, etc.)
#
# > Hello!
# Hi! How can I help you today?
#
# > What is Haskell?
# Haskell is a purely functional programming language...
#
# > exit
# Goodbye!
```

**Supported Keybindings:**
- `C-a` / `Home` - Move to beginning of line
- `C-e` / `End` - Move to end of line
- `C-b` / `←` - Move backward one character
- `C-f` / `→` - Move forward one character
- `C-k` - Kill (delete) to end of line
- `C-d` / `Delete` - Delete character under cursor
- `C-h` / `Backspace` - Delete character before cursor
- `C-w` - Delete word backwards
- `C-u` - Delete from cursor to beginning of line
- `↑` / `↓` - Navigate command history

Exit the interactive mode by typing `exit`, `quit`, `:q`, or pressing `Ctrl-D`.

### Command-line Options

- `-p, --provider PROVIDER` - LLM provider (openai, claude, ollama, gemini) [required]
- `-m, --model MODEL` - Model name (e.g., gpt-4o-mini, claude-3-5-sonnet-20241022)
- `-k, --api-key API_KEY` - API key for the provider
- `-u, --base-url BASE_URL` - Base URL for the provider (mainly for Ollama, default: localhost:11434)
- `-s, --stream` - Enable streaming output (real-time response)

### Environment Variables

Instead of passing API keys via command-line, you can set them as environment variables:

- `OPENAI_API_KEY` - For OpenAI
- `ANTHROPIC_API_KEY` - For Claude
- `GEMINI_API_KEY` - For Gemini

### Examples

```bash
# Using a specific model
echo "Write a Python function" | llm-hs-exe -p openai -m gpt-4o

# With API key from command line
echo "Hello" | llm-hs-exe -p claude -k "your-api-key-here"

# Using environment variable
export OPENAI_API_KEY="your-api-key"
echo "Generate a poem" | llm-hs-exe -p openai

# Ollama with custom base URL
echo "Explain recursion" | llm-hs-exe -p ollama -u "localhost:11434"

# Pipe from file
cat prompt.txt | llm-hs-exe -p claude

# Chain with other commands
echo "List 5 programming languages" | llm-hs-exe -p gemini | grep "1."

# Streaming output (real-time response) - ALL PROVIDERS SUPPORTED!
echo "Tell me a story" | llm-hs-exe -p openai --stream
echo "Explain quantum computing" | llm-hs-exe -p claude --stream
echo "Write a haiku" | llm-hs-exe -p ollama --stream
echo "What is functional programming?" | llm-hs-exe -p gemini --stream
```

### Streaming Support

**Real-time streaming output is now supported for ALL providers!** ✨

- ✅ **OpenAI** - Full SSE streaming support
- ✅ **Claude** - Full SSE streaming support
- ✅ **Ollama** - Full streaming support
- ✅ **Gemini** - Full SSE streaming support

When streaming is enabled with the `--stream` or `-s` flag, the response will be output in real-time as it's generated:

```bash
# Short form
echo "Write a poem about Haskell" | llm-hs-exe -p openai -s

# Long form
echo "Explain monads" | llm-hs-exe -p claude --stream

# Works with all providers
echo "Tell me a joke" | llm-hs-exe -p gemini -s
```

### Default Models

- OpenAI: `gpt-4o-mini`
- Claude: `claude-3-5-sonnet-20241022`
- Ollama: `llama3.2`
- Gemini: `gemini-1.5-flash`

## Development

```bash
# Build
stack build

# Run tests
stack test

# Run
stack exec llm-hs-exe -- --provider openai < input.txt
```

## License

BSD-3-Clause
