from string_with_arrows import string_with_arrows
import string

################################

# Token types ✧˖°
TT_INT = '✧ NUM ✧'
TT_FLOAT = '★,KINDA_NUM★'
TT_PLUS = '･ﾟ☆ PLUS(HIE) ☆･ﾟ'
TT_MINUS = '✧ MINUS ✧'
TT_MUL = '･ﾟ★ TIMES ★･ﾟ'
TT_DIV = '･ﾟ★ SLASH ★･ﾟ'
TT_LPAREN = '*:･ﾟ LEFT_PAW *:･ﾟ'
TT_RPAREN = '*:･ﾟ RIGHT_PAW *:･ﾟ'
TT_EOF = '✿ EOF ✿'
TT_POW = '✧ POWER ✧'
TT_ID = '☆ ID ☆'
TT_KEYWORD = '⧣₊˚ KEYWORD ⧣₊˚'
TT_EQ = '𓂃★ EQUALS ★𓂃'
TT_LT = '⧣₊˚ LESS_THAN ⧣₊˚'
TT_GT = '⧣₊˚ GREATER_THAN ⧣₊˚'
TT_LTE = '⧣₊˚ LESS_THAN_EQUAL ⧣₊˚'
TT_GTE = '⧣₊˚ GREATER_THAN_EQUAL ⧣₊˚'
TT_NE = '⧣₊˚ NOT_EQUAL ⧣₊˚'
TT_EE = '⧣₊˚ DOUBLE_EQUAL ⧣₊˚'
# - end of Token Types - #
################################


################################
# ✿ KEYWORDS ✿
KEYWORDS = ['var', '<33', '<3', '</3', 'if', 'elif',
            'else', 'for', 'to', 'step', 'while', 'fun', 'then']
################################


################################
# ✧･ﾟ: *✧･ﾟ:* Constants *:･ﾟ✧*:･ﾟ✧
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS
# - end of const >.< - #
################################


################################
# ｡ﾟ(ﾟ´ω`ﾟ)ﾟ｡ Error Classes ｡ﾟ(ﾟ´ω`ﾟ)ﾟ｡
class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}\n'
        result += string_with_arrows(
            self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end,
                         '(◕︿◕✿) Oopsie! We found a mischief character', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end,
                         '(´｡• ω •｡`) Uh-oh! Wrong syntax', details)


class RuntimeError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end,
                         '(ノ﹏ヽ) Oh no! Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result = f'{self.error_name}: {self.details}\n'
        result += string_with_arrows(
            self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        context = self.context
        while context:
            result = f'  File {pos.fn}, line {
                str(pos.ln + 1)}, in {context.display_name}\n' + result
            pos = context.parent_entry_pos
            context = context.parent
        return '( ╹ -╹) \nTraceback (most recent call last):\n' + result
# - end of errors :( - #
################################


################################
# ✧˖°˖☆ Position Class ☆˖°˖✧
class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0
        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)
# - end of Position Class - #
################################


################################
# ♡♥ Token Class ♥♡
class KawaiiToken:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        self.pos_start = pos_start
        self.pos_end = pos_end

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value:
            return f'✧･ﾟ: *✧･ﾟ:{self.type}:{self.value}:･ﾟ✧*:･ﾟ✧'
        return f'✧･ﾟ: *✧･ﾟ:{self.type}:･ﾟ✧*:･ﾟ✧'
# - end of class KawaiiToken - #
################################


################################
# ✿❀ Lexer Class ❀✿ #
class CyuteLexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(
            self.text) else None

    def make_tokens(self):
        tokens = []
        while self.current_char is not None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS or self.current_char == '.':
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(KawaiiToken(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(KawaiiToken(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(KawaiiToken(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(KawaiiToken(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(KawaiiToken(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(KawaiiToken(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(KawaiiToken(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char in LETTERS:
                tokens.append(self.make_id())
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, f"'{char}'")
        tokens.append(KawaiiToken(TT_EOF, pos_start=self.pos))
        return tokens, None

    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE

        return KawaiiToken(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char is not None and (self.current_char in DIGITS or self.current_char == '.'):
            if self.current_char == '.':
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += "."
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return KawaiiToken(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return KawaiiToken(TT_FLOAT, float(num_str), pos_start, self.pos)

    def make_id(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_char is not None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()

        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_ID
        return KawaiiToken(tok_type, id_str, pos_start, self.pos)

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '>':
            self.advance()
            return KawaiiToken(TT_NE, pos_start=pos_start, pos_end=self.pos), None
        self.advance()
        return None, InvalidSyntaxError(pos_start, self.pos, "(ó﹏ò｡) I expected a '>' after '<'")

    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE

            return KawaiiToken(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = TT_LTE
        return KawaiiToken(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = TT_GTE
        return KawaiiToken(tok_type, pos_start=pos_start, pos_end=self.pos)
        # - end of Lexer ✧˖° - #
################################


################################
# ʕ•ᴥ•ʔ Node Classes ʕ•ᴥ•ʔ
class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'


class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end


class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end


class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'


class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'
# - end of Node Classes - #
################################


################################
# (づ｡◕‿‿◕｡)づ ParseResult
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count += res.advance_count
        if res.error:
            self.error = res.error
        return res.node

    def success(self, node_):
        self.node = node_
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self
# - end of ParseResult Class - #
################################


################################
# ✿❀ Parser Class ❀✿
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        self.update_current_tok()

    def update_current_tok(self):
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        else:
            self.current_tok = None

    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "(ó﹏ò｡) I expected a number, identifier, operator or parenthesis"))
        return res

    def nachos(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type == TT_INT or tok.type == TT_FLOAT:
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TT_ID:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            node = res.register(self.expr())
            if res.error:
                return res

            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(node)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "(ó﹏ò｡) I expected a ')'"))

        return res.failure(InvalidSyntaxError(
            self.current_tok.pos_start, self.current_tok.pos_end, "(ó﹏ò｡) I expected a number, identifier, operator or parenthesis"))

    def power(self):
        return self.bin_op(self.nachos, (TT_POW, ), self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type == TT_PLUS or tok.type == TT_MINUS:
            res.register_advancement()
            self.advance()
            node = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok, node))
        return self.power()

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))

    def math_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def compare_expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD, '</3'):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            node = res.register(self.compare_expr())
            if res.error:
                return res
            return res.success(UnaryOpNode(op_tok, node))
        node = res.register(self.bin_op(
            self.math_expr, (TT_LT, TT_GT, TT_LTE, TT_GTE, TT_EE, TT_NE)))
        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "(ó﹏ò｡) I expected a number, identifier, operator or parenthesis"))

        return res.success(node)

    def expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD, 'var'):
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_ID:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, '(ó﹏ò｡) I expected an identifier'))
            var_name = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, '(ó﹏ò｡) I expected an ='))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.bin_op(self.compare_expr,
                            ((TT_KEYWORD, "<33"), (TT_KEYWORD, "<3"))))
        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "(ó﹏ò｡) I expected a number, identifier, operator or parenthesis"))

        return res.success(node)

    def bin_op(self, func_x, ops, func_y=None):
        if func_y is None:
            func_y = func_x
        res = ParseResult()
        left = res.register(func_x())
        if res.error:
            return res

        while self.current_tok is not None and (self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_y())
            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)
# - end of Parser Class - #
################################


################################
# ✧･ﾟ: *✧･ﾟ:* Runtime *:･ﾟ✧*:･ﾟ✧
class RuntimeResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error:
            self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self
# - end of RuntimeResult Class - #
################################


################################
# ⋆ ˚｡⋆୨୧˚ Number Class ˚｡⋆ ⋆୨୧˚
class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end

        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def set_context(self, context=None):
        self.context = context
        return self

    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RuntimeError(
                    other.pos_start, other.pos_end, '【・_・?】You can\'t divide by zero', self.context)
            return Number(self.value / other.value).set_context(self.context), None

    def powered_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value == other.value else 'Nuh uh').set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value != other.value else 'Nuh uh').set_context(self.context), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value < other.value else 'Nuh uh').set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value > other.value else 'Nuh uh').set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value <= other.value else 'Nuh uh').set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value >= other.value else 'Nuh uh').set_context(self.context), None

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value and other.value else 'Nuh uh').set_context(self.context), None

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number('Uh Huh' if self.value or other.value else 'Nuh uh').set_context(self.context), None

    def notted(self):
        return Number('Uh Huh' if self.value == 0 else 'Nuh uh').set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f'{self.value}'
# - end of Number Class - #
################################


################################
# ☆*:.｡.o(≧▽≦)o.｡.:*☆ Interpreter
class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_NumberNode(self, node, context):
        return RuntimeResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_VarAccessNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)
        if not value:
            return res.failure(RuntimeError(node.pos_start, node.pos_end, f'(´｡• ω •｡`) {var_name} isn\'t defined', context))
        value = value.copy().set_pos(node.pos_start, node.pos_end)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error:
            return res
        context.symbol_table.set(var_name, value)
        return res.success(value.set_pos(node.pos_start, node.pos_end))

    def visit_BinOpNode(self, node, context):
        res = RuntimeResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.right_node, context))
        if res.error:
            return res

        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.divided_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powered_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, '<33'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, '<3'):
            result, error = left.ored_by(right)
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RuntimeResult()
        number = res.register(self.visit(node.node, context))
        if res.error:
            return res

        error = None
        if node.op_tok.type == TT_PLUS:
            result, error = number.multed_by(Number(1))
        elif node.op_tok.type == TT_MINUS:
            result, error = number.multed_by(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD, '</3'):
            number, error = number.notted()
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))
# - end of Interpreter Class - #
################################


################################
# ✿❀ Context ❀✿
class Context:
    def __init__(self, dn, parent=None, parent_entry_pos=None):
        self.display_name = dn
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None
# - end of Context Class - #
################################


################################
# ⋆.ೃ࿔*:･ Symbol Table ೃ࿔*:･⋆
class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name, None)
        if value is None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]
# - end of Symbol Table - #
################################


################################
# (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ Run Function
global_symbol_table = SymbolTable()
global_symbol_table.set("NaN", Number(0))
global_symbol_table.set("YAY", Number(1))
global_symbol_table.set("NAY", Number(0))


def run(fn, text):
    lexer = CyuteLexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error:
        return None, error

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)
    return result.value, result.error
# - end of Run Function - #
