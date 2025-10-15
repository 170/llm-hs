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
- Configurable via command-line arguments, environment variables, or configuration file
- Configuration file support for default settings (`~/.llm-hs.json`)

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
- `-c, --color MODE` - Color mode (auto, always, never) - default: auto

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

# Color output examples
echo "Hello, world!" | llm-hs-exe -p openai --color always  # Force color output
echo "Hello, world!" | llm-hs-exe -p claude --color never   # Disable color output
llm-hs-exe -p openai --color auto  # Auto-detect terminal color support (default)
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

### Color Output

**Colored output makes it easier to distinguish between different parts of the conversation!** 🎨

The tool supports colored output with three modes:

- **auto** (default): Automatically detects if the terminal supports color
- **always**: Always use colored output, even when piping to a file
- **never**: Disable colored output

Color scheme:
- **System messages** (cyan): Informational messages and instructions
- **User messages** (green): Your input messages (in conversation history)
- **Assistant messages** (blue): LLM responses
- **Error messages** (red): Error and warning messages
- **Info messages** (yellow): Headers and status information
- **Tool messages** (magenta): MCP tool execution details

```bash
# Use default auto-detection
llm-hs-exe -p openai

# Force color output (useful when piping)
llm-hs-exe -p claude --color always

# Disable colors (useful for logging)
llm-hs-exe -p openai --color never
```

### Default Models

- OpenAI: `gpt-4o-mini`
- Claude: `claude-3-5-sonnet-20241022`
- Ollama: `llama3.2`
- Gemini: `gemini-1.5-flash`

## Configuration File

You can create a configuration file at `~/.llm-hs.json` to set default values for your LLM provider, model, and other options. Command-line arguments will override these defaults.

### Example Configuration

```json
{
  "provider": "claude",
  "model": "claude-3-5-sonnet-20241022",
  "stream": true,
  "color": "auto",
  "apiKey": "your-api-key-here",
  "baseUrl": "http://localhost:11434"
}
```

### Configuration Options

- `provider` (string): Default LLM provider (`"openai"`, `"claude"`, `"ollama"`, `"gemini"`)
- `model` (string): Default model name for the provider
- `stream` (boolean): Enable streaming output by default
- `color` (string): Color mode (`"auto"`, `"always"`, `"never"`) - default: `"auto"`
- `apiKey` (string): API key for the provider (optional, can use environment variables instead)
- `baseUrl` (string): Base URL for the provider (mainly for Ollama)
- `mcpServers` (array): MCP server configurations (see MCP Support section below)

### Priority Order

Settings are applied in the following order (later takes precedence):

1. Default values (hardcoded in the application)
2. Configuration file (`~/.llm-hs.json`)
3. Environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc.)
4. Command-line arguments

### Example Usage with Config File

If you have `~/.llm-hs.json` with default provider and model:

```bash
# Uses config file defaults
llm-hs-exe

# Override provider from config
llm-hs-exe --provider openai

# Override model from config
llm-hs-exe --model gpt-4o
```

## MCP (Model Context Protocol) Support

llm-hs now supports MCP servers, which allow LLMs to access external tools and resources. MCP servers are configured in the `~/.llm-hs.json` configuration file.

### What is MCP?

MCP (Model Context Protocol) is a standardized protocol for connecting AI assistants to external data sources and tools. MCP servers can provide:
- **Tools**: Functions that the LLM can describe in its responses
- **Resources**: Access to files, databases, APIs, and other data sources

### Configuring MCP Servers

Add MCP servers to your configuration file:

```json
{
  "provider": "claude",
  "model": "claude-3-5-sonnet-20241022",
  "stream": true,
  "mcpServers": [
    {
      "name": "filesystem",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/allowed/directory"]
    },
    {
      "name": "github",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"]
    }
  ]
}
```

### MCP Server Configuration Format

Each MCP server entry has the following fields:

- `name` (string): A friendly name for the server
- `command` (string): The command to execute (e.g., `"npx"`, `"python"`, `"node"`)
- `args` (array of strings): Arguments to pass to the command

### How MCP Works with llm-hs

When you start llm-hs with MCP servers configured:

1. **Startup**: llm-hs automatically starts all configured MCP servers
2. **Discovery**: The tool queries each server for available tools and resources
3. **Context**: Available tools are added to the LLM's system prompt
4. **Interaction**: The LLM can describe tools in its responses based on your queries
5. **Cleanup**: MCP servers are automatically stopped when the session ends

### Example MCP Servers

Here are some popular MCP servers you can use:

#### Filesystem Server
Access local files and directories:
```json
{
  "name": "filesystem",
  "command": "npx",
  "args": ["-y", "@modelcontextprotocol/server-filesystem", "/Users/username/Documents"]
}
```

#### GitHub Server
Query GitHub repositories:
```json
{
  "name": "github",
  "command": "npx",
  "args": ["-y", "@modelcontextprotocol/server-github"]
}
```

#### Custom MCP Server
You can also create your own MCP server in any language:
```json
{
  "name": "my-tool",
  "command": "python",
  "args": ["my_mcp_server.py"]
}
```

### MCP in Interactive Mode

When using interactive mode with MCP servers, you'll see a message indicating how many tools are available:

```
=== Interactive Mode ===
Provider: Claude
Model: claude-3-5-sonnet-20241022 (default)
MCP Tools: 5 tools available
Type your message and press Enter. Type 'exit' or 'quit' to end.
```

The LLM can then reference and describe these tools when responding to your queries.

### Notes

- MCP servers run as child processes and communicate via stdio (JSON-RPC)
- All MCP servers are automatically stopped when llm-hs exits
- If an MCP server fails to start, llm-hs will display an error but continue running
- Currently, the LLM can see and describe tools but cannot directly execute them (this is a safety feature)

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
