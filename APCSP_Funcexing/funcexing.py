# Pan Lin
# Mr. Thierfelder
# AP Computer Science Principles
# 9 April 2020
import re
from typing import List, Optional, Union, Tuple

# standard examples
eg_f = 'f(x)=cos(e^x-x^2)*ln(x/2)'  # classic
eg_g = 'g(x)=(sin(x-e^(x^(x-1))+1)/x)+(x*e^(x+1)+1)-1'  # lots of parentheses
eg_a = 'a(t)=cos(e^t-t^2)*ln(t/2)'  # a different independent variable
eg_b = 'b(x)=(sin(x-e^(x^(n-1))+1)/n)+m'  # some unbounded constant
eg_F = 'F(x,y)=(sin(x-e^(y^(x-1))+1)/y)+x'  # multivariate function
# non-standard examples
eg_h = 'h(x) = sin(x *  cos(x ))* x^2 + e^x + 1'  # unnecessary spaces
eg_c = 'c(x)=(cos((e^x)-(x^2))*(ln(x/2)))+x*2^x'  # unnecessary parentheses
eg_cc = '(cos((e^x)-(x^2))*(ln(x/2)))+x*2^x'
eg_G = 'G=(sin(x-e^(x^(n-1))+1)/n)+m'  # The independent variable is 'x' by default
eg_ = 'cos(e^x-x^2)*ln(x/2)'  # The name of the function will be None
# wrong examples
eg_p = 'p(x)=cos(e^x-x^2)*ln(x/2))'  # unbalanced parentheses


class FuncNode(object):
    regex_first_operator = re.compile(r'^-?(?P<before_opr>[^+*/^()-]+)(?P<opr>[+*/^-])(?=\()')
    regex_pre_naked_subs = re.compile(r'\w+(?=[+*/^-])|(?<=[+*/^-])\w|^\w$')
    regex_wrapping_func_name = re.compile(r'^(?P<name>[A-Za-z]+)\((?P<arg>.+)\)$')

    def __init__(self, name: str, left=None, right=None):
        assert isinstance(left, FuncNode) or left is None
        assert isinstance(right, FuncNode) or right is None
        self.value: str = name
        self.left: Optional[FuncNode] = left
        self.right: Optional[FuncNode] = right

    @classmethod
    def from_expr(cls, expr: str):
        name: str
        left: Optional[FuncNode]
        right: Optional[FuncNode]
        srs: List[Tuple[int, int]] = cls.subs_ranges(expr)
        while len(srs) == 1 and expr[0] == '(' and expr[-1] == ')':  # strip the outermost redundant parentheses ^(...)$
            expr = expr[1:len(expr)-1]
            srs = cls.subs_ranges(expr)
        if len(srs) == 1:
            right = None
            if expr[-1] == ')':
                name = cls.regex_wrapping_func_name.match(expr).group('name')
                left = cls.from_expr(cls.regex_wrapping_func_name.match(expr).group('arg'))
            else:
                name = expr
                left = None
        else:
            split: int = -1
            try:
                for opr in [['+', '-'], ['*', '/'], ['^']]:
                    for sr in srs[:-1]:
                        if expr[sr[1]] in opr:
                            split = sr[1]
                            raise IndexError
            except IndexError:
                pass
            assert split > 0
            name = expr[split]
            print("left:", expr[:split])
            print("right:", expr[split+1:])
            left = cls.from_expr(expr[:split])
            right = cls.from_expr(expr[split+1:])
        assert name is not None
        return FuncNode(name, left, right)

    @classmethod
    def subs_ranges(cls, expr: str) -> List[Tuple[int, int]]:
        stats: int = 0
        wrapped_subs: List[Union[Tuple[int, int], Tuple[int]]] = []
        for index, char in enumerate(expr):
            assert stats >= 0
            if char == "(":
                if not wrapped_subs or len(wrapped_subs[-1]) == 2:
                    wrapped_subs.append((index,))
                stats += 1
            elif char == ")":
                stats -= 1
                if stats == 0 and len(wrapped_subs[-1]) == 1:
                    wrapped_subs[-1] += (index + 1,)
        assert not wrapped_subs or len(wrapped_subs[-1]) == 2

        pre_naked_subs = cls.regex_pre_naked_subs.finditer(expr)
        naked_subs = []
        if wrapped_subs:
            for sub in pre_naked_subs:
                for index, pr in enumerate(wrapped_subs):
                    if sub.start() > pr[0] and sub.end() < pr[1]:
                        break
                    elif index + 1 == len(wrapped_subs):
                        naked_subs.append(sub.span())
        else:
            return [sub.span() for sub in pre_naked_subs]
        return sorted(naked_subs + wrapped_subs)


class FuncEx(object):
    regex_heading = re.compile(
        r'^(?P<head>(?P<name>[a-zA-z]+)\((?P<parameters>(?:[a-z],)*[a-z])\)=)?(?P<expression>.*)$')

    def __init__(self, origin_str: str):
        std_str = re.sub(r'\s', '', origin_str)
        head: Optional = self.__class__.regex_heading.match(std_str)
        self._name: Optional[str] = head.group('name')
        self._vars: List[str] = head.group('parameters').split(',') if head is None else ['x']
        self._expr: str = head.group('expression')
        assert self._expr
        self._root: FuncNode = FuncNode.from_expr(self._expr)

    @property
    def name(self) -> str:
        return self._name

    @property
    def vars(self) -> List[str]:
        return self._vars

    @property
    def expr(self) -> str:
        return self._expr


if __name__ == '__main__':
    print(FuncNode.from_expr(eg_cc), len(eg_cc))
