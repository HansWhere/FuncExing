# Pan Lin
# Mr. Thierfelder
# AP Computer Science Principles
# 9 April 2020
# (independent development)
import re
import math
from typing import List, Optional, Union, Tuple, Dict, Callable

# standard examples
eg_f = 'f(x)=cos(e^x-x^2)*ln(x/2)'  # classic
eg_g = 'g(x)=-(sin(x-e^(x^(x-1))+1)/x)+(x*e^(x+1)+1)-1'  # lots of parentheses
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


class Func(object):
    constants: Dict[str, Union[int, float]] = {
        'e': math.e,
        'pi': math.pi
    }

    def __init__(self, name: Union[str, float], function: Callable, derivative: Callable):
        if isinstance(name, str):
            try:
                name = float(name)
            except ValueError:
                pass
        self._name: Union[float, str] = name
        self._apply: Callable = function
        self._derivative: Callable[[str], Func] = derivative

    @classmethod
    def constant(cls, cons: float):
        def cons_func(*args):
            assert args is not None
            return cons

        def d_cons_func(var):
            assert isinstance(var, str)
            return 0

        return Func(cons, cons_func, d_cons_func)

    @property
    def name(self):
        return self._name

    @property
    def isnumeric(self):
        return isinstance(self.name, float)

    @property
    def isconstant(self):
        return self.isnumeric or self.name in self.__class__.constants

    def __call__(self, *args):
        return self._apply(*args)

    def derivative(self, var: str):
        return self._derivative(var)


class FuncNode(object):
    regex_pre_naked_subs = re.compile(r'\w+(?=[,+*/^-])|(?:(?<=[,+*/^-])|^)\w+$')
    regex_wrapping_func_name = re.compile(r'^(?P<name>[A-Za-z]+|-)\((?P<args>.+)\)$')

    def __init__(self, name: str, children: Optional[list] = None):
        if children is None:
            children = []
        self._name: str = name
        self._children: List[FuncNode] = children

    @property
    def name(self):
        return self._name

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self, new_children):
        assert isinstance(new_children, FuncNode) or new_children is None
        self._children = new_children

    @property
    def tuple_tree(self):
        if self.children:
            return (self.name,) + tuple(child.tuple_tree for child in self.children)
        else:
            return self.name

    def apply(self, **kwargs):
        pass

    @classmethod
    def from_expr(cls, expr: str):
        name: Optional[str] = None
        children: Optional[List[FuncNode]] = []
        splits: List[int] = [-1]
        # To make sure expr[splits[i] + 1: splits[i + 1]] is expr[:splits[i]] if i == 0

        srs: List[Tuple[int, int]] = cls.subs_ranges(expr)
        while len(srs) == 1 and expr[0] == '(' and expr[-1] == ')':
            # strip the outermost redundant parentheses ^(...)$
            expr = expr[1:len(expr) - 1]
            srs = cls.subs_ranges(expr)

        if len(srs) == 1:
            if expr[-1] == ')':
                wfm = cls.regex_wrapping_func_name.match(expr)
                name = wfm.group('name')
                wfm_args = wfm.group('args')
                wfm_args_subs = cls.subs_ranges(wfm_args)
                commas = re.finditer(',', wfm_args)
                if list(commas):
                    for comma in commas:
                        c_pos = comma.start()
                        try:
                            for sub in wfm_args_subs:
                                if sub[0] < c_pos < sub[1]:
                                    raise IndexError
                            splits.append(comma.start())
                        except IndexError:
                            pass
                    for i in range(len(splits) - 1):
                        children.append(cls.from_expr(expr[splits[i] + 1: splits[i + 1]]))
                else:
                    children.append(cls.from_expr(wfm_args))
            else:
                name = expr
                children = None
        else:
            for opr in [['+', '-'], ['*', '/'], ['^']]:
                for sr in srs[:-1]:
                    selected_opr = expr[sr[1]]
                    if selected_opr in opr:
                        if name is None:
                            if selected_opr not in ['+', '-'] and expr[0] == '-':
                                # special condition: negative function
                                return FuncNode('-', [cls.from_expr(expr[1:])])
                            name = selected_opr
                        if expr[sr[1]] == name:
                            splits.append(sr[1])
                if len(splits) > 1:
                    break
            splits.append(len(expr))
            for i in range(len(splits) - 1):
                children.append(cls.from_expr(expr[splits[i] + 1: splits[i + 1]]))
        assert name is not None
        return FuncNode(name, children)

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
        self._root: FuncNode = FuncNode.from_expr(self.expr)

    @property
    def name(self) -> str:
        return self._name

    @property
    def vars(self) -> List[str]:
        return self._vars

    @property
    def expr(self) -> str:
        return self._expr

    @property
    def root(self) -> FuncNode:
        return self._root

    @property
    def tuple_tree(self) -> tuple:
        return self.root.tuple_tree

    def apply(self, *args, **kwargs):
        assert len(args) <= len(self.vars)


if __name__ == '__main__':
    print(FuncEx(eg_g).tuple_tree)
