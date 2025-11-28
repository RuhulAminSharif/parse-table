#include <bits/stdc++.h>
using namespace std;

const string EPS = "ε";

// ==========================================
// 0. SYMBOL TABLE STRUCTURE
// ==========================================
struct SymbolInfo
{
  string name;
  string type;
  int scope;
  int offset;
};

class SymbolTable
{
private:
  map<string, SymbolInfo> table;
  int currentOffset = 0;

public:
  // Returns status string instead of printing directly
  string insert(string name, string type, int scope)
  {
    if (table.find(name) != table.end())
    {
      return "Error: " + name + " already declared";
    }
    int size = (type == "int") ? 2 : 4;
    table[name] = {name, type, scope, currentOffset};
    string msg = "INSERT: " + name + " (" + type + ", off:" + to_string(currentOffset) + ")";
    currentOffset += size;
    return msg;
  }

  // Returns status string instead of printing directly
  string lookup(string name)
  {
    if (table.find(name) != table.end())
    {
      SymbolInfo info = table[name];
      return "LOOKUP: " + name + " (" + info.type + ")";
    }
    else
    {
      return "Error: " + name + " undeclared";
    }
  }
};

// ==========================================
// 1. GRAMMAR TRANSFORMATION FUNCTIONS
// ==========================================

// Check if a non-terminal has immediate left recursion
bool hasImmediateLeftRecursion(const string &nonTerminal,
                               const vector<string> &productions)
{
  for (const string &prod : productions)
  {
    if (!prod.empty())
    {
      stringstream ss(prod);
      string firstSymbol;
      ss >> firstSymbol;
      if (firstSymbol == nonTerminal)
        return true;
    }
  }
  return false;
}

// Remove immediate left recursion for a single non-terminal
pair<vector<string>, pair<string, vector<string>>> removeImmediateLeftRecursion(
    const string &nonTerminal, const vector<string> &productions)
{
  vector<string> alpha; // Left recursive productions (A -> A α)
  vector<string> beta;  // Non-left recursive productions (A -> β)
  for (const string &prod : productions)
  {
    if (prod.empty() || prod == EPS)
    {
      beta.push_back(prod);
      continue;
    }
    stringstream ss(prod);
    string firstSymbol;
    ss >> firstSymbol;

    if (firstSymbol == nonTerminal)
    {
      // A -> A α, extract α
      string remaining = prod.substr(firstSymbol.length());
      while (!remaining.empty() && isspace(remaining.front()))
        remaining.erase(remaining.begin());
      if (remaining.empty())
        remaining = EPS;
      alpha.push_back(remaining);
    }
    else
    {
      beta.push_back(prod);
    }
  }

  string newNT = nonTerminal + "'";
  vector<string> updatedProductions;
  vector<string> newNTProductions;

  // A -> β A' for each β
  for (const string &b : beta)
  {
    if (b == EPS)
      updatedProductions.push_back(newNT);
    else
      updatedProductions.push_back(b + " " + newNT);
  }

  // A' -> α A' for each α
  for (const string &a : alpha)
  {
    if (a == EPS)
      newNTProductions.push_back(newNT);
    else
      newNTProductions.push_back(a + " " + newNT);
  }
  // A' -> ε
  newNTProductions.push_back(EPS);

  return {updatedProductions, {newNT, newNTProductions}};
}

// Remove all left recursion from grammar
vector<pair<string, vector<string>>> removeLeftRecursion(
    vector<pair<string, vector<string>>> &grammar, ofstream &outfile)
{
  vector<pair<string, vector<string>>> result;
  int n = grammar.size();
  outfile << "=== LEFT RECURSION REMOVAL ===\n\n";
  for (int i = 0; i < n; i++)
  {
    string Ai = grammar[i].first;
    vector<string> productions = grammar[i].second;
    // Eliminate indirect left recursion: substitute Aj in Ai (j < i)
    for (int j = 0; j < i; j++)
    {
      string Aj = grammar[j].first;
      vector<string> newProductions;

      for (const string &prod : productions)
      {
        if (prod.empty())
        {
          newProductions.push_back(prod);
          continue;
        }
        stringstream ss(prod);
        string firstSymbol;
        ss >> firstSymbol;
        if (firstSymbol == Aj)
        {
          // Replace Aj with its productions
          string remaining = prod.substr(firstSymbol.length());
          while (!remaining.empty() && isspace(remaining.front()))
            remaining.erase(remaining.begin());

          for (const string &ajProd : grammar[j].second)
          {
            if (ajProd == EPS)
              newProductions.push_back(remaining.empty() ? EPS : remaining);
            else if (remaining.empty())
              newProductions.push_back(ajProd);
            else
              newProductions.push_back(ajProd + " " + remaining);
          }
        }
        else
        {
          newProductions.push_back(prod);
        }
      }
      productions = newProductions;
    }
    // Remove immediate left recursion
    if (hasImmediateLeftRecursion(Ai, productions))
    {
      outfile << "Removing left recursion from " << Ai << ":\n";
      auto [updatedProds, newNTPair] =
          removeImmediateLeftRecursion(Ai, productions);

      outfile << "  " << Ai << " -> ";
      for (size_t k = 0; k < updatedProds.size(); k++)
      {
        outfile << updatedProds[k];
        if (k < updatedProds.size() - 1)
          outfile << " | ";
      }
      outfile << "\n";

      outfile << "  " << newNTPair.first << " -> ";
      for (size_t k = 0; k < newNTPair.second.size(); k++)
      {
        outfile << newNTPair.second[k];
        if (k < newNTPair.second.size() - 1)
          outfile << " | ";
      }
      outfile << "\n\n";

      result.push_back({Ai, updatedProds});
      result.push_back(newNTPair);
      grammar[i].second = updatedProds; // Update for further processing
    }
    else
    {
      outfile << "No left recursion in " << Ai << "\n\n";
      result.push_back({Ai, productions});
      grammar[i].second = productions;
    }
  }

  return result;
}

string commonPrefix(const string &s1, const string &s2)
{
  string prefix = "";
  for (size_t i = 0; i < min(s1.size(), s2.size()); i++)
  {
    if (s1[i] == s2[i])
      prefix += s1[i];
    else
      break;
  }
  return prefix;
}

vector<pair<string, vector<string>>> removeLeftFactoring(
    vector<pair<string, vector<string>>> &grammar, ofstream &outfile)
{
  vector<pair<string, vector<string>>> newGrammar;
  map<string, int> ntCount;

  outfile << "=== LEFT FACTORING ===\n\n";

  for (auto &rule : grammar)
  {
    string nonTerminal = rule.first;
    vector<string> productions = rule.second;
    set<int> used;
    vector<string> newProds;
    vector<pair<string, vector<string>>> tempNewNTs;

    for (int i = 0; i < (int)productions.size(); i++)
    {
      if (used.count(i))
        continue;

      used.insert(i);
      vector<string> group = {productions[i]};

      // Group productions with common prefix
      for (int j = i + 1; j < (int)productions.size(); j++)
      {
        if (used.count(j))
          continue;
        string prefix = commonPrefix(productions[i], productions[j]);
        if (!prefix.empty())
        {
          group.push_back(productions[j]);
          used.insert(j);
        }
      }

      if (group.size() > 1)
      {
        // Find longest common prefix
        string prefix = group[0];
        for (size_t k = 1; k < group.size(); k++)
          prefix = commonPrefix(prefix, group[k]);

        outfile << "Left factoring " << nonTerminal << " with prefix: \""
                << prefix << "\"\n";

        // Generate new non-terminal name
        string newNonTerminal = nonTerminal + "''";
        if (ntCount.count(nonTerminal))
        {
          ntCount[nonTerminal]++;
          for (int cnt = 1; cnt < ntCount[nonTerminal]; cnt++)
            newNonTerminal += "'";
        }
        else
        {
          ntCount[nonTerminal] = 1;
        }

        newProds.push_back(prefix + " " + newNonTerminal);

        vector<string> newNTprods;
        for (auto &prod : group)
        {
          string suffix = prod.substr(prefix.size());
          while (!suffix.empty() && isspace(suffix.front()))
            suffix.erase(suffix.begin());

          if (suffix.empty())
            suffix = EPS;
          newNTprods.push_back(suffix);
        }

        tempNewNTs.push_back({newNonTerminal, newNTprods});
      }
      else
      {
        newProds.push_back(productions[i]);
      }
    }

    newGrammar.push_back({nonTerminal, newProds});
    for (auto &nt : tempNewNTs)
      newGrammar.push_back(nt);
  }

  outfile << "\n";
  return newGrammar;
}

bool isTerminal(const string &symbol, const set<string> &nonTerminals)
{
  return nonTerminals.find(symbol) == nonTerminals.end() && symbol != EPS;
}

// ==========================================
// 2. FIRST & FOLLOW SETS
// ==========================================

map<string, set<string>> computeFirstSets(
    const vector<pair<string, vector<string>>> &grammar)
{
  map<string, set<string>> firstSets;
  set<string> nonTerminals;

  for (const auto &rule : grammar)
    nonTerminals.insert(rule.first);

  for (const auto &nt : nonTerminals)
    firstSets[nt] = set<string>();

  bool changed = true;
  while (changed)
  {
    changed = false;

    for (const auto &rule : grammar)
    {
      string nonTerminal = rule.first;
      for (const string &production : rule.second)
      {
        if (production.empty() || production == EPS)
        {
          if (firstSets[nonTerminal].find(EPS) ==
              firstSets[nonTerminal].end())
          {
            firstSets[nonTerminal].insert(EPS);
            changed = true;
          }
          continue;
        }

        stringstream ss(production);
        string symbol;
        bool allHaveEpsilon = true;

        while (ss >> symbol)
        {
          if (isTerminal(symbol, nonTerminals))
          {
            if (firstSets[nonTerminal].find(symbol) ==
                firstSets[nonTerminal].end())
            {
              firstSets[nonTerminal].insert(symbol);
              changed = true;
            }
            allHaveEpsilon = false;
            break;
          }
          else if (symbol != EPS)
          {
            bool hasEpsilon = false;
            for (const string &first : firstSets[symbol])
            {
              if (first == EPS)
              {
                hasEpsilon = true;
                continue;
              }
              if (firstSets[nonTerminal].find(first) ==
                  firstSets[nonTerminal].end())
              {
                firstSets[nonTerminal].insert(first);
                changed = true;
              }
            }

            if (!hasEpsilon)
            {
              allHaveEpsilon = false;
              break;
            }
          }
        }

        if (allHaveEpsilon)
        {
          if (firstSets[nonTerminal].find(EPS) ==
              firstSets[nonTerminal].end())
          {
            firstSets[nonTerminal].insert(EPS);
            changed = true;
          }
        }
      }
    }
  }
  return firstSets;
}

// Compute FIRST set for a string of symbols
set<string> computeFirstOfString(const string &str,
                                 const map<string, set<string>> &firstSets,
                                 const set<string> &nonTerminals)
{
  set<string> result;
  if (str.empty() || str == EPS)
  {
    result.insert(EPS);
    return result;
  }

  stringstream ss(str);
  string symbol;
  bool allHaveEpsilon = true;

  while (ss >> symbol)
  {
    if (isTerminal(symbol, nonTerminals))
    {
      result.insert(symbol);
      allHaveEpsilon = false;
      break;
    }
    else if (symbol != EPS && firstSets.count(symbol))
    {
      bool hasEpsilon = false;
      for (const string &first : firstSets.at(symbol))
      {
        if (first == EPS)
        {
          hasEpsilon = true;
          continue;
        }
        result.insert(first);
      }

      if (!hasEpsilon)
      {
        allHaveEpsilon = false;
        break;
      }
    }
  }
  if (allHaveEpsilon)
    result.insert(EPS);
  return result;
}

// Compute FOLLOW sets for all non-terminals
map<string, set<string>> computeFollowSets(
    const vector<pair<string, vector<string>>> &grammar,
    const map<string, set<string>> &firstSets)
{
  map<string, set<string>> followSets;
  set<string> nonTerminals;
  string startSymbol;

  // Collect all non-terminals
  for (const auto &rule : grammar)
  {
    nonTerminals.insert(rule.first);
    if (startSymbol.empty())
      startSymbol = rule.first; // First non-terminal is start symbol
  }

  // Initialize FOLLOW sets
  for (const auto &nt : nonTerminals)
    followSets[nt] = set<string>();

  // Rule 1: Add $ to FOLLOW of start symbol
  followSets[startSymbol].insert("$");
  bool changed = true;
  while (changed)
  {
    changed = false;

    for (const auto &rule : grammar)
    {
      string nonTerminal = rule.first;

      for (const string &production : rule.second)
      {
        if (production.empty() || production == EPS)
          continue;

        // Parse production into symbols
        vector<string> symbols;
        stringstream ss(production);
        string symbol;
        while (ss >> symbol)
          symbols.push_back(symbol);

        // Process each symbol in the production
        for (size_t i = 0; i < symbols.size(); i++)
        {
          string currentSymbol = symbols[i];

          // Only process non-terminals
          if (!isTerminal(currentSymbol, nonTerminals) &&
              currentSymbol != EPS)
          {
            // Rule 2: If A -> αBβ, add FIRST(β) - {ε} to FOLLOW(B)
            if (i + 1 < symbols.size())
            {
              // Get remaining string β
              string beta = "";
              for (size_t j = i + 1; j < symbols.size(); j++)
              {
                if (!beta.empty())
                  beta += " ";
                beta += symbols[j];
              }

              set<string> firstOfBeta =
                  computeFirstOfString(beta, firstSets, nonTerminals);
              bool hasEpsilon = false;

              for (const string &first : firstOfBeta)
              {
                if (first == EPS)
                {
                  hasEpsilon = true;
                  continue;
                }
                if (followSets[currentSymbol].find(first) ==
                    followSets[currentSymbol].end())
                {
                  followSets[currentSymbol].insert(first);
                  changed = true;
                }
              }

              // Rule 3: If A -> αB or A -> αBβ where ε ∈ FIRST(β), add
              // FOLLOW(A) to FOLLOW(B)
              if (hasEpsilon || i == symbols.size() - 1)
              {
                for (const string &follow : followSets[nonTerminal])
                {
                  if (followSets[currentSymbol].find(follow) ==
                      followSets[currentSymbol].end())
                  {
                    followSets[currentSymbol].insert(follow);
                    changed = true;
                  }
                }
              }
            }
            else
            {
              // Rule 3: If A -> αB, add FOLLOW(A) to FOLLOW(B)
              for (const string &follow : followSets[nonTerminal])
              {
                if (followSets[currentSymbol].find(follow) ==
                    followSets[currentSymbol].end())
                {
                  followSets[currentSymbol].insert(follow);
                  changed = true;
                }
              }
            }
          }
        }
      }
    }
  }
  return followSets;
}

// Build LL(1) parsing table
map<pair<string, string>, string> buildParsingTable(
    const vector<pair<string, vector<string>>> &grammar,
    const map<string, set<string>> &firstSets,
    const map<string, set<string>> &followSets,
    const set<string> &nonTerminals)
{
  map<pair<string, string>, string> table;
  for (const auto &rule : grammar)
  {
    string A = rule.first;
    int productionNum = 0;
    for (const string &production : rule.second)
    {
      productionNum++;
      set<string> firstSet =
          computeFirstOfString(production, firstSets, nonTerminals);

      // For each terminal in FIRST(production) - {ε}
      for (const string &terminal : firstSet)
      {
        if (terminal != EPS)
        {
          pair<string, string> key = {A, terminal};
          if (table.count(key))
          {
            // Conflict detected
            table[key] = "CONFLICT";
          }
          else
          {
            table[key] = A + " -> " + production;
          }
        }
      }

      // If ε ∈ FIRST(production), add FOLLOW(A) entries
      if (firstSet.count(EPS))
      {
        for (const string &terminal : followSets.at(A))
        {
          pair<string, string> key = {A, terminal};
          if (table.count(key))
          {
            table[key] = "CONFLICT";
          }
          else
          {
            table[key] = A + " -> " + production;
          }
        }
      }
    }
  }
  return table;
}

// Print parsing table in tabular format
void printParsingTable(const map<pair<string, string>, string> &table,
                       const set<string> &nonTerminals,
                       const set<string> &terminals, ofstream &outfile)
{
  outfile << "\n=== LL(1) PARSING TABLE ===\n\n";

  int colWidth = 18;
  int ntColWidth = 16;
  // Helper function to get display length
  auto getDisplayLength = [](const string &s) -> size_t
  {
    size_t len = 0;
    for (size_t i = 0; i < s.length();)
    {
      unsigned char c = s[i];
      if (c < 0x80)
      { // ASCII
        len++;
        i++;
      }
      else if (c < 0xE0)
      { // 2-byte UTF-8
        len++;
        i += 2;
      }
      else if (c < 0xF0)
      { // 3-byte UTF-8
        len++;
        i += 3;
      }
      else
      { // 4-byte UTF-8
        len++;
        i += 4;
      }
    }
    return len;
  };

  // Helper function to pad string to desired width
  auto padString = [&](const string &s, int width) -> string
  {
    size_t displayLen = getDisplayLength(s);
    if (displayLen >= (size_t)width)
    {
      return s;
    }
    return s + string(width - displayLen, ' ');
  };
  vector<string> terminalList(terminals.begin(), terminals.end());
  vector<string> nonTerminalList(nonTerminals.begin(), nonTerminals.end());
  // Print top border
  outfile << "+";
  outfile << string(ntColWidth - 2, '-');
  outfile << "+";
  for (size_t i = 0; i < terminalList.size(); i++)
  {
    outfile << string(colWidth - 1, '-');
    outfile << "+";
  }
  outfile << "\n";
  // Print header with terminals
  outfile << "|" << padString("NON-TERMINAL", ntColWidth - 2) << "|";
  for (const auto &term : terminalList)
  {
    outfile << padString(term, colWidth - 1) << "|";
  }
  outfile << "\n";
  // Print middle border
  outfile << "+";
  outfile << string(ntColWidth - 2, '-');
  outfile << "+";
  for (size_t i = 0; i < terminalList.size(); i++)
  {
    outfile << string(colWidth - 1, '-');
    outfile << "+";
  }
  outfile << "\n";
  // Print table rows
  for (const auto &nt : nonTerminalList)
  {
    outfile << "|" << padString(nt, ntColWidth - 2) << "|";
    for (const auto &term : terminalList)
    {
      pair<string, string> key = {nt, term};
      string cellContent = "";
      if (table.count(key))
      {
        cellContent = table.at(key);
        size_t displayLen = getDisplayLength(cellContent);
        if (displayLen > (size_t)(colWidth - 2))
        {
          cellContent = cellContent.substr(0, colWidth - 5) + "...";
        }
      }
      outfile << padString(cellContent, colWidth - 1) << "|";
    }
    outfile << "\n";
    // Print bottom border
    outfile << "+";
    outfile << string(ntColWidth - 2, '-');
    outfile << "+";

    for (size_t i = 0; i < terminalList.size(); i++)
    {
      outfile << string(colWidth - 1, '-');
      outfile << "+";
    }
    outfile << "\n";
  }
}

// ==========================================
// 3. PARSING TRACE LOGIC & SYMBOL TABLE INTEGRATION
// ==========================================

// Helper: Tokenize input string (Better handling of delimiters)
vector<string> tokenize(string input)
{
  vector<string> tokens;
  string current = "";
  for (char c : input)
  {
    if (isspace(c))
    {
      if (!current.empty())
      {
        tokens.push_back(current);
        current = "";
      }
    }
    else if (isalnum(c) || c == '_' || c == '.')
    {
      // Keep alphanumeric, underscore, dot together
      current += c;
    }
    else
    {
      // It's an operator or punctuation (like ; or + or *)
      // Push existing token first
      if (!current.empty())
      {
        tokens.push_back(current);
        current = "";
      }
      // Push the operator as a distinct token
      tokens.push_back(string(1, c));
    }
  }
  if (!current.empty())
    tokens.push_back(current);
  tokens.push_back("$"); // Add end marker
  return tokens;
}

// Helper: Map raw input token to Grammar Terminal with Priority
// Priority:
// 1. Exact match in terminals (e.g., "int", "float", ";", "+")
// 2. Numbers (digits) -> "num"
// 3. Identifiers (variables) -> "id"
string getGrammarToken(const string &token, const set<string> &terminals)
{
  // 1. Is it a specific keyword or symbol defined in grammar?
  if (terminals.count(token))
  {
    return token;
  }
  if (token == "$")
    return "$";

  // 2. Is it a number?
  // Simple check: start with digit
  if (isdigit(token[0]))
  {
    return "num";
  }

  // 3. Is it an identifier?
  // Starts with letter or underscore, and wasn't found in terminals list
  if (isalpha(token[0]) || token[0] == '_')
  {
    return "id";
  }

  // Fallback
  return token;
}

void traceInput(const string &inputStr,
                map<pair<string, string>, string> &parsingTable,
                const string &startSymbol, const set<string> &terminals,
                ofstream &outfile)
{
  vector<string> tokens = tokenize(inputStr);
  stack<string> parseStack;
  parseStack.push("$");
  parseStack.push(startSymbol);

  // Initialize Symbol Table
  SymbolTable symTable;
  string currentDeclarationType = ""; // To track type during declaration parsing

  outfile << "\n=== PARSING TRACE ===\n";
  outfile << "Input String: " << inputStr << "\n\n";

  // Formatting widths
  int stackWidth = 30;
  int inputWidth = 30;
  int actionWidth = 30;

  outfile << left << setw(stackWidth) << "Stack"
          << left << setw(inputWidth) << "Input"
          << left << setw(actionWidth) << "Action"
          << "Symbol Table / Semantic Action"
          << "\n";
  outfile << string(stackWidth + inputWidth + actionWidth + 35, '-') << "\n";

  int ip = 0; // Input pointer

  while (!parseStack.empty())
  {
    string top = parseStack.top();
    string currentInput = tokens[ip];

    // Map actual input to grammar terminal
    string grammarToken = getGrammarToken(currentInput, terminals);

    // 1. Capture visual string for Stack (Before Modification)
    string stackStr = "";
    stack<string> temp = parseStack;
    vector<string> tempVec;
    while (!temp.empty())
    {
      tempVec.push_back(temp.top());
      temp.pop();
    }
    for (auto it = tempVec.rbegin(); it != tempVec.rend(); ++it)
      stackStr += *it + " ";

    // 2. Capture visual string for Input (Before Modification)
    string inputRemaining = "";
    for (int i = ip; i < tokens.size(); i++)
      inputRemaining += tokens[i] + " ";

    string actionLog = "";
    string symLog = "";

    // Logic
    if (top == grammarToken)
    {
      // --- SYMBOL TABLE INTEGRATION LOGIC ---
      if (grammarToken == "int" || grammarToken == "float")
      {
        currentDeclarationType = grammarToken; // Store type
      }
      else if (grammarToken == "id")
      {
        if (!currentDeclarationType.empty())
        {
          // We are inside a declaration (e.g., int x)
          symLog = symTable.insert(currentInput, currentDeclarationType, 0); // Scope 0 for global
        }
        else
        {
          // We are using a variable (e.g., x + y)
          symLog = symTable.lookup(currentInput);
        }
      }
      else if (grammarToken == ";")
      {
        currentDeclarationType = ""; // Reset type at end of declaration statement
      }
      // --------------------------------------

      // MATCH
      if (top == "$")
      {
        actionLog = "ACCEPTED";
        outfile << left << setw(stackWidth) << stackStr
                << left << setw(inputWidth) << inputRemaining
                << left << setw(actionWidth) << actionLog
                << symLog << "\n";
        break;
      }

      actionLog = "Match " + currentInput;
      parseStack.pop();
      ip++;
    }
    else if (terminals.count(top) || top == "$")
    {
      actionLog = "ERROR: Expected " + top + ", found " + currentInput;
      outfile << left << setw(stackWidth) << stackStr
              << left << setw(inputWidth) << inputRemaining
              << left << setw(actionWidth) << actionLog
              << symLog << "\n";
      return;
    }
    else
    {
      // NON-TERMINAL: Look up table
      pair<string, string> key = {top, grammarToken};

      if (parsingTable.count(key) && parsingTable[key] != "CONFLICT")
      {
        string prodStr = parsingTable[key];
        actionLog = prodStr;

        parseStack.pop(); // Pop Non-terminal

        // Extract right-hand side symbols
        string rhs = prodStr.substr(prodStr.find("->") + 2);

        // Split RHS into symbols
        stringstream ss(rhs);
        string symbol;
        vector<string> symbols;
        while (ss >> symbol)
          symbols.push_back(symbol);

        // Push symbols to stack in REVERSE order
        for (auto it = symbols.rbegin(); it != symbols.rend(); ++it)
        {
          if (*it != EPS && *it != "ε")
          { // Don't push Epsilon
            parseStack.push(*it);
          }
        }
      }
      else
      {
        actionLog = "ERROR: No rule for [" + top + ", " + grammarToken + "]";
        outfile << left << setw(stackWidth) << stackStr
                << left << setw(inputWidth) << inputRemaining
                << left << setw(actionWidth) << actionLog
                << symLog << "\n";
        return;
      }
    }

    // Print the row for this step
    outfile << left << setw(stackWidth) << stackStr
            << left << setw(inputWidth) << inputRemaining
            << left << setw(actionWidth) << actionLog
            << symLog << "\n";
  }
}

// ==========================================
// 4. MAIN FUNCTION
// ==========================================

int main()
{
  // Ensure this file exists with the grammar rules
  ifstream infile("./input/input.txt");
  if (!infile.is_open())
  {
    cerr << "Error: Could not open input file\n";
    return 1;
  }

  ofstream outfile("./output/output.txt");
  if (!outfile.is_open())
  {
    cerr << "Error: Could not create output file\n";
    return 1;
  }

  vector<pair<string, vector<string>>> grammar;
  string line;
  bool isFirst = true;
  string statement;
  while (getline(infile, line))
  {
    if (line.empty())
      continue;
    // Special handling: Assume 1st line might be the test string in some formats,
    // but typically grammar files contain rules.
    // If the file strictly contains grammar, we keep reading.
    // If you have a separate input string in the file, logic needs adjustment.
    // Assuming standard grammar file format for this block:
    if (line.find("->") == string::npos)
    {
      // If line doesn't have ->, it might be the input string
      statement = line;
      continue;
    }

    string left = line.substr(0, line.find("->"));
    string right = line.substr(line.find("->") + 2);

    auto trim = [](string s)
    {
      s.erase(remove_if(s.begin(), s.end(), ::isspace), s.end());
      return s;
    };

    auto trimKeepSpace = [](string s)
    {
      while (!s.empty() && isspace(s.front()))
        s.erase(s.begin());
      while (!s.empty() && isspace(s.back()))
        s.pop_back();
      return s;
    };

    left = trim(left);

    vector<string> productions;
    stringstream ss(right);
    string prod;
    while (getline(ss, prod, '|'))
    {
      productions.push_back(trimKeepSpace(prod));
    }

    grammar.push_back({left, productions});
  }
  infile.close();

  // Step 1: Remove left recursion
  auto noRecursionGrammar = removeLeftRecursion(grammar, outfile);

  // Step 2: Remove left factoring
  auto factoredGrammar = removeLeftFactoring(noRecursionGrammar, outfile);

  outfile << "=== FINAL GRAMMAR ===\n\n";
  for (auto &rule : factoredGrammar)
  {
    outfile << rule.first << " -> ";
    for (size_t i = 0; i < rule.second.size(); i++)
    {
      outfile << rule.second[i];
      if (i + 1 < rule.second.size())
        outfile << " | ";
    }
    outfile << "\n";
  }

  // Step 3: Compute FIRST sets
  auto firstSets = computeFirstSets(factoredGrammar);

  outfile << "\n=== FIRST SETS ===\n\n";
  for (const auto &entry : firstSets)
  {
    outfile << "FIRST(" << entry.first << ") = { ";
    bool first = true;
    for (const string &symbol : entry.second)
    {
      if (!first)
        outfile << ", ";
      outfile << symbol;
      first = false;
    }
    outfile << " }\n";
  }

  // Step 4: Compute FOLLOW sets
  auto followSets = computeFollowSets(factoredGrammar, firstSets);
  outfile << "\n=== FOLLOW SETS ===\n\n";
  for (const auto &entry : followSets)
  {
    outfile << "FOLLOW(" << entry.first << ") = { ";
    bool first = true;
    for (const string &symbol : entry.second)
    {
      if (!first)
        outfile << ", ";
      outfile << symbol;
      first = false;
    }
    outfile << " }\n";
  }

  // Step 5: Build LL(1) Parsing Table
  set<string> nonTerminals;
  set<string> terminals;

  for (const auto &rule : factoredGrammar)
    nonTerminals.insert(rule.first);

  // Extract terminals from grammar
  for (const auto &rule : factoredGrammar)
  {
    for (const string &prod : rule.second)
    {
      stringstream ss(prod);
      string symbol;
      while (ss >> symbol)
      {
        if (isTerminal(symbol, nonTerminals) && symbol != EPS)
          terminals.insert(symbol);
      }
    }
  }

  // Add $ as end marker
  terminals.insert("$");

  auto parsingTable =
      buildParsingTable(factoredGrammar, firstSets, followSets, nonTerminals);
  printParsingTable(parsingTable, nonTerminals, terminals, outfile);

  // Step 6: Parsing Trace
  // We use the string provided in your requirement
  if (statement.empty())
    statement = "int x ; float y ; x + y * 3"; // Default if not found in file

  // Safely find Start Symbol (First rule of the final grammar)
  if (!factoredGrammar.empty())
  {
    string startSymbol = factoredGrammar[0].first;
    traceInput(statement, parsingTable, startSymbol, terminals, outfile);
  }
  else
  {
    outfile << "Error: Grammar is empty." << endl;
  }

  cout << "Processing complete. Check output file." << endl;

  outfile.close();
  return 0;
}