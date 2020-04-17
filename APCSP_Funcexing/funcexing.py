# Pan Lin
# Mr. Thierfelder
# AP Computer Science Principles
# 9 April 2020
import re
import time
from typing import List, Optional, Union, Tuple

# standard examples
eg_f = 'f(x)=cos(e^x-x^2)*ln(x/2)'  # classic
eg_g = 'g(x)=(sin(x-e^(x^(x-1))+1)/x)+(x*e^(x+1)+1)-1'  # lots of parentheses
eg_a = 'a(t)=cos(e^t-t^2)*ln(t/2)'  # a different independent variable
eg_b = 'b(x)=(sin(x-e^(x^(n-1))+1)/n)+m'  # some unbounded constant
eg_F = 'F(x,y)=(sin(x-e^(y^(x-1))+1)/y)+x'  # multivariate function
# non-standard examples
eg_h = 'h(x) = sin(x *  cos(x ))* x^2 + e^x + 1'  # unnecessary spaces
eg_c = 'c(x)=(cos((e^x)-(x^2))*(ln(x/2)))'  # unnecessary parentheses
eg_cc = '((cos((e^x)-(x^2))*(ln(x/2))))'
eg_G = 'G=(sin(x-e^(x^(n-1))+1)/n)+m'  # The independent variable is 'x' by default
eg_ = 'cos(e^x-x^2)*ln(x/2)'  # The name of the function will be None
# wrong examples
eg_p = 'p(x)=cos(e^x-x^2)*ln(x/2))'  # unbalanced parentheses


def time_stats(func):
    def decorated_func(*args, **kwargs):
        previous_time = time.time()
        func(*args, **kwargs)
        print(time.time(), previous_time)

    return decorated_func()


class Singleton(type):
    def __init__(cls, *args, **kwargs):
        cls.__instance = None
        super().__init__(*args, **kwargs)

    def __call__(cls, *args, **kwargs):
        if cls.__instance is None:
            cls.__instance = super().__call__(*args, **kwargs)
        return cls.__instance


class FuncNode(object):
    regex_first_operator = re.compile(r'^-?(?P<before_opr>[^+*/^()-]+)(?P<opr>[+*/^-])(?=\()')
    regex_group = re.compile(r'([A-Za-z]+)(?=[+*/^-])')

    def __init__(self, name: str, left=None, right=None):
        assert isinstance(left, FuncNode) and isinstance(right, FuncNode)
        self.value: str = name
        self.left: Optional[FuncNode] = left
        self.right: Optional[FuncNode] = right

    @classmethod
    def from_expr(cls, expr: str):
        name: str
        fpp: Tuple[int, int] = cls.parentheses_pairs(expr)[0]
        while fpp[0] == 0 and fpp[1] == len(expr):  # strip the outermost redundant parentheses     ^(...)$
            expr = expr.strip(r'[()]')
            fpp = cls.parentheses_pairs(expr)[0]
        if fpp:  # The name will be the operator right after the first group of expression
            name = expr[fpp[0] + 1]
        else:  # The name will be the first operator (+, -, *, /, ^)
            name = cls.regex_first_operator.match(expr).group(1)
        assert name is not None
        left: Optional[FuncNode] = None
        right: Optional[FuncNode] = None
        return FuncNode(name, left, right)

    @staticmethod
    def parentheses_pairs(expr: str) -> List[Tuple[int, int]]:
        stats: int = 0
        pairs: List[Union[Tuple[int, int], Tuple[int]]] = []
        print(len(expr), expr)
        for index, char in enumerate(expr):
            assert stats >= 0
            if char == "(":
                if not pairs or len(pairs[-1]) == 2:
                    pairs.append((index,))
                stats += 1
            elif char == ")":
                stats -= 1
                if stats == 0 and len(pairs[-1]) == 1:
                    pairs[-1] += (index,)
            print(index, char, '~~', stats, pairs)
        assert not pairs or len(pairs[-1]) == 2
        return pairs


class FuncEx(object):
    regex_heading = re.compile(
        r'^(?P<head>(?P<name>[a-zA-z]+)\((?P<parameters>(?:[a-z],)*[a-z])\)=)?(?P<expression>.*)$')

    def __init__(self, origin_str: str):
        std_str = re.sub(r'\s', '', origin_str)
        head: Optional = self.__class__.regex_heading.match(std_str)
        self._name: Optional[str] = head.group('name')
        self._parameters: List[str] = head.group('parameters').split(',') if head is None else ['x']
        self._expr: str = head.group('expression')
        assert self._expr
        self._root: FuncNode = FuncNode.from_expr(self._expr)

    @property
    def name(self) -> str:
        return self._name

    @property
    def parameters(self) -> List[str]:
        return self._parameters

    @property
    def expr(self) -> str:
        return self._expr


if __name__ == '__main__':
    print(FuncNode.parentheses_pairs(eg_c), len(eg_cc))
