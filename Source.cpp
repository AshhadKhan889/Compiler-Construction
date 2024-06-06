#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <unordered_map>
#include <cctype>
#include <unordered_set>

using namespace std;

enum TokenType {
    INT,
    DOUBLE,
    NEW,
    SWITCH,
    ELSE,
    BREAK,
    ENUM,
    CASE,
    FLOAT,
    CHAR,
    FOR,
    RETURN,
    IF,
    VOID,
    DELETE,
    DO,
    WHILE,
    IDENTIFIER,
    NUMBER,
    CHAR_LITERAL,
    ASSIGN,
    PLUS,
    SEMICOLON,
    MAIN,
    END
};

struct Token {
    TokenType type;
    string value;
};

class Lexer {
public:
    Lexer(const string& source) : source(source), position(0) {}

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (position < source.size()) {
            char current = source[position];
            if (isspace(current)) {
                position++;
            }
            else if (current == '+') {
                tokens.push_back({ PLUS, "+" });
                position++;
            }
            else if (current == '=') {
                tokens.push_back({ ASSIGN, "=" });
                position++;
            }
            else if (current == ';') {
                tokens.push_back({ SEMICOLON, ";" });
                position++;
            }
            else if (isdigit(current)) {
                tokens.push_back(number());
            }
            else if (isalpha(current)) {
                tokens.push_back(identifier());
            }
            else if (current == '\'') {
                tokens.push_back(charLiteral());
            }
            else {
                cerr << "Unknown character: " << current << endl;
                position++;
            }
        }
        tokens.push_back({ END, "" });
        return tokens;
    }

private:
    Token number() {
        string value;
        while (position < source.size() && isdigit(source[position])) {
            value += source[position++];
        }
        return { NUMBER, value };
    }

    Token identifier() {
        string value;
        while (position < source.size() && isalnum(source[position])) {
            value += source[position++];
        }

        static const unordered_map<string, TokenType> keywords = {
            {"int", INT}, {"double", DOUBLE}, {"new", NEW}, {"switch", SWITCH},
            {"else", ELSE}, {"break", BREAK}, {"enum", ENUM}, {"case", CASE},
            {"float", FLOAT}, {"char", CHAR}, {"for", FOR}, {"return", RETURN},
            {"if", IF}, {"void", VOID}, {"delete", DELETE}, {"do", DO}, {"while", WHILE},
            {"main", MAIN }
        };

        if (keywords.count(value)) {
            return { keywords.at(value), value };
        }
        return { IDENTIFIER, value };
    }

    Token charLiteral() {
        string value;
        value += source[position++]; // skip opening '
        if (position < source.size() && source[position] != '\'') {
            value += source[position++];
            if (position < source.size() && source[position] == '\'') {
                value += source[position++];
            }
            else {
                cerr << "Unterminated character literal" << endl;
            }
        }
        else {
            cerr << "Empty character literal" << endl;
        }
        return { CHAR_LITERAL, value };
    }

    string source;
    size_t position;
};

struct ASTNode {
    virtual ~ASTNode() = default;
};

struct NumberNode : ASTNode {
    string value;
    explicit NumberNode(const string& value) : value(value) {}
};

struct CharLiteralNode : ASTNode {
    string value;
    explicit CharLiteralNode(const string& value) : value(value) {}
};

struct IdentifierNode : ASTNode {
    string name;
    explicit IdentifierNode(const string& name) : name(name) {}
};

struct BinaryOperationNode : ASTNode {
    string op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    BinaryOperationNode(string op, unique_ptr<ASTNode> left, unique_ptr<ASTNode> right)
        : op(op), left(move(left)), right(move(right)) {}
};

struct AssignmentNode : ASTNode {
    unique_ptr<IdentifierNode> identifier;
    unique_ptr<ASTNode> expression;

    AssignmentNode(unique_ptr<IdentifierNode> identifier, unique_ptr<ASTNode> expression)
        : identifier(move(identifier)), expression(move(expression)) {}
};

struct DeclarationNode : ASTNode {
    string type;
    unique_ptr<AssignmentNode> assignment;

    DeclarationNode(string type, unique_ptr<AssignmentNode> assignment)
        : type(type), assignment(move(assignment)) {}
};

struct ProgramNode : ASTNode {
    vector<unique_ptr<ASTNode>> declarations;
};

class Parser {
public:
    explicit Parser(const vector<Token>& tokens) : tokens(tokens), position(0) {}

    unique_ptr<ProgramNode> parse() {
        unique_ptr<ProgramNode> program = make_unique<ProgramNode>();
        while (!isAtEnd()) {
            if (auto declaration = parseDeclaration()) {
                program->declarations.push_back(move(declaration));
            }
            else {
                cerr << "Parsing error: unexpected token at position " << position << endl;
                return nullptr;
            }
        }
        return program;
    }

private:
    unique_ptr<ASTNode> parseDeclaration() {
        if (match(INT) || match(DOUBLE) || match(NEW) || match(SWITCH) || match(ELSE) ||
            match(BREAK) || match(ENUM) || match(CASE) || match(FLOAT) || match(CHAR) ||
            match(FOR) || match(RETURN) || match(IF) || match(VOID) || match(DELETE) ||
            match(DO) || match(WHILE) || match(MAIN)) {
            string type = previous().value;
            auto identifier = make_unique<IdentifierNode>(consume(IDENTIFIER, "Expect identifier").value);
            consume(ASSIGN, "Expect '=' after variable name");
            auto expression = parseExpression();
            consume(SEMICOLON, "Expect ';' after expression");
            return make_unique<DeclarationNode>(type, make_unique<AssignmentNode>(move(identifier), move(expression)));
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseExpression() {
        return parseTerm();
    }

    unique_ptr<ASTNode> parseTerm() {
        auto node = parseFactor();
        while (match(PLUS)) {
            string op = previous().value;
            auto right = parseFactor();
            node = make_unique<BinaryOperationNode>(op, move(node), move(right));
        }
        return node;
    }

    unique_ptr<ASTNode> parseFactor() {
        if (match(NUMBER)) {
            return make_unique<NumberNode>(previous().value);
        }
        if (match(CHAR_LITERAL)) {
            return make_unique<CharLiteralNode>(previous().value);
        }
        if (match(IDENTIFIER)) {
            return make_unique<IdentifierNode>(previous().value);
        }
        cerr << "Unexpected token: " << peek().value << endl;
        return nullptr;
    }

    bool match(TokenType type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }

    bool check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().type == type;
    }

    Token advance() {
        if (!isAtEnd()) position++;
        return previous();
    }

    bool isAtEnd() {
        return peek().type == END;
    }

    Token peek() {
        return tokens[position];
    }

    Token previous() {
        return tokens[position - 1];
    }

    Token consume(TokenType type, const string& message) {
        if (check(type)) return advance();
        cerr << message << endl;
        return {};
    }

    const vector<Token>& tokens;
    size_t position;
};

class SemanticAnalyzer {
public:
    void analyze(ProgramNode* root) {
        enterScope();
        for (auto& decl : root->declarations) {
            if (auto declaration = dynamic_cast<DeclarationNode*>(decl.get())) {
                analyzeDeclaration(declaration);
            }
        }
        exitScope();
    }

private:
    void enterScope() {
        symbolTableStack.push_back({});
    }

    void exitScope() {
        symbolTableStack.pop_back();
    }

    void analyzeDeclaration(DeclarationNode* node) {
        string varName = node->assignment->identifier->name;
        string varType = node->type;

        if (symbolTableStack.back().count(varName)) {
            cerr << "Error: Variable '" << varName << "' already declared in this scope." << endl;
        }
        else {
            symbolTableStack.back()[varName] = varType;
        }

        if (!isValidAssignment(varType, node->assignment->expression.get())) {
            cerr << "Error: Type mismatch in assignment to variable '" << varName << "'." << endl;
        }

        analyzeExpression(node->assignment->expression.get());
    }

    bool isValidAssignment(const string& varType, ASTNode* expr) {
        if (auto numberNode = dynamic_cast<NumberNode*>(expr)) {
            if (varType == "int" || varType == "double" || varType == "float") {
                return true;
            }
            return false;
        }
        if (auto charLiteralNode = dynamic_cast<CharLiteralNode*>(expr)) {
            if (varType == "char") {
                return true;
            }
            return false;
        }
        if (auto identifierNode = dynamic_cast<IdentifierNode*>(expr)) {
            // Further type checking for variables can be implemented here
            return true; // Assuming identifiers are properly typed
        }
        return false;
    }

    void analyzeExpression(ASTNode* node) {
        if (auto binOp = dynamic_cast<BinaryOperationNode*>(node)) {
            analyzeExpression(binOp->left.get());
            analyzeExpression(binOp->right.get());
        }
        else if (auto ident = dynamic_cast<IdentifierNode*>(node)) {
            if (!isDeclared(ident->name)) {
                cerr << "Error: Variable '" << ident->name << "' not declared." << endl;
            }
        }
        else if (auto number = dynamic_cast<NumberNode*>(node)) {
            // Number node is valid
        }
        else if (auto charLiteral = dynamic_cast<CharLiteralNode*>(node)) {
            // Char literal node is valid
        }
        else {
            cerr << "Unknown AST node type" << endl;
        }
    }

    bool isDeclared(const string& name) {
        for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it) {
            if (it->count(name)) {
                return true;
            }
        }
        return false;
    }

    vector<unordered_map<string, string>> symbolTableStack;
};

string tokenTypeToString(const TokenType& type) {
    switch (type) {
    case INT: return "Keyword";
    case DOUBLE: return "Keyword";
    case NEW: return "Keyword";
    case SWITCH: return "Keyword";
    case ELSE: return "Keyword";
    case BREAK: return "Keyword";
    case ENUM: return "Keyword";
    case CASE: return "Keyword";
    case FLOAT: return "Keyword";
    case CHAR: return "Keyword";
    case FOR: return "Keyword";
    case RETURN: return "Keyword";
    case IF: return "Keyword";
    case VOID: return "Keyword";
    case DELETE: return "Keyword";
    case DO: return "Keyword";
    case WHILE: return "Keyword";
    case IDENTIFIER: return "Variable";
    case NUMBER: return "Number";
    case CHAR_LITERAL: return "CharLiteral";
    case ASSIGN: return "Operator";
    case PLUS: return "Operator";
    case SEMICOLON: return "Semicolon";
    case MAIN: return "Keyword";
    case END: return "End";
    default: return "Unknown";
    }
}

int main() {
    string source = "   char a = 'c'; int a = 10; ";

    Lexer lexer(source);
    vector<Token> tokens = lexer.tokenize();

    cout << "Tokens generated by the lexer:" << endl;
    for (const auto& token : tokens) {
        string tokenType = tokenTypeToString(token.type);
        cout << "[" << tokenType << "]: \"" << token.value << "\"" << endl;
    }

    Parser parser(tokens);
    auto syntaxTree = parser.parse();

    if (syntaxTree) {
        cout << "Parsing completed successfully." << endl;

        SemanticAnalyzer analyzer;
        analyzer.analyze(syntaxTree.get());
        cout << "Semantic analysis completed." << endl;
    }
    else {
        cout << "Parsing failed." << endl;
    }

    return 0;
}
