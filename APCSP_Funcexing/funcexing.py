# Pan Lin
# Mr. Thierfelder
# AP Computer Science Principles
# 9 April 2020
# (independent development)
import re
import math
from abc import ABCMeta, abstractmethod, ABC
from functools import reduce
from typing import List, Optional, Union, Tuple, Dict, Callable, Any, Match


eg_f = 'f(x)=cos(e^x-x^2)*ln(x/2)'  # classic
eg_g = 'g(x)=-(sin(x-e^(x^(x-1))+1)/x)+(x*e^(x+1)+1)-1'  # lots of parentheses
eg_c = 'c(x)=(cos((e^x)-(x^2))*(ln(x/2)))+x*2^x'  # unnecessary parentheses
eg_cc = '(cos((e^x)-(x^2))*(ln(x/2)))+x*2^x'


class IFunc(metaclass=ABCMeta):
    def __init__(self, *args: 'IFunc'):
        assert all(isinstance(arg, IFunc) for arg in args)
        self._vars: Tuple[IFunc, ...] = args

    @property
    def vars(self) -> Tuple['IFunc', ...]:
        return self._vars

    @property
    @abstractmethod
    def val(self) -> Union[str, int, float]:
        pass

    @abstractmethod
    def derivative(self, var: str) -> 'IFunc':
        pass

    def apply(self, **kwargs: 'IFunc') -> 'IFunc':
        new_func: IFunc = self.__new__(self.__class__)
        new_func.__init__(*(var.apply(**kwargs) for var in self.vars))
        return new_func

    def __call__(self, **kwargs: 'IFunc') -> 'IFunc':
        return self.apply(**kwargs)

    def __eq__(self, other: 'IFunc') -> bool:
        # It is almost impossible to check whether two elementary function are equal
        # So this only represents that the two functions are formally equal
        return self.__class__ == other.__class__ and self.vars == other.vars

    def __str__(self) -> str:
        return '({})'.format(str(self.val))


class UnaryMixIn(IFunc, ABC):
    def __init__(self, arg: 'IFunc'):
        assert isinstance(arg, IFunc)
        self._vars: Tuple[IFunc, ...] = (arg,)

    @property
    def var(self):
        return self.vars[0]


class Element(IFunc):

    def __init__(self, val: Union[str, int, float]):
        self._val: Union[str, int, float] = val
        self._vars = None

    @property
    def val(self) -> Union[str, int, float]:
        return self._val

    def apply(self, **kwargs: 'IFunc') -> 'IFunc':
        return kwargs[self.val] if self.val in kwargs else None

    def derivative(self, var: str) -> 'IFunc':
        return Element(1 if self.val == var else 0)

    def __eq__(self, other: 'IFunc') -> bool:
        return self.val == other.val

    def __str__(self) -> str:
        return str(self.val)


class Add(IFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return sum(var.val for var in self.vars)
        else:
            return '+'.join(str(var.val) for var in self.vars)

    def derivative(self, var: str):
        return Add(*(v.derivative(var) for v in self.vars))

    def __eq__(self, other: 'IFunc') -> bool:
        return self.__class__ == other.__class__ and \
               all(var in other.vars for var in self.vars) and \
               all(var in self.vars for var in other.vars)


class Negative(IFunc, UnaryMixIn):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return -self.var.val
        else:
            return '(-{})'.format(str(self.var.val))

    def derivative(self, var: str):
        return Negative(self.var.derivative(var))


class Multiply(IFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return reduce(lambda x, y: x * y, (var.val for var in self.vars))
        else:
            return '*'.join(str(var.val) for var in self.vars)

    def derivative(self, var: str):
        vars_: Tuple['IFunc'] = self.vars
        return Add(*(vars_[:n] + vars_[n].derivative + vars_[n + 1:] for n in range(len(vars_))))

    def __eq__(self, other: 'IFunc') -> bool:
        return self.__class__ == other.__class__ and \
               all(var in other.vars for var in self.vars) and \
               all(var in self.vars for var in other.vars)


class Reciprocal(IFunc, UnaryMixIn):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return -self.var.val
        else:
            return '1/({})'.format(str(self.var.val))

    def derivative(self, var: str):
        return Negative(Multiply(self.var.derivative(var), Reciprocal(Power(self.var, Element(2)))))


class Power(IFunc):
    def __init__(self, base: IFunc, *nested_exponents: IFunc):
        assert isinstance(base, IFunc) and all(isinstance(index, IFunc) for index in nested_exponents)
        self._vars: Tuple[IFunc, IFunc] = (base, Power(*nested_exponents))

    @property
    def base(self):
        return self._vars[0]

    @property
    def exponent(self):
        return self._vars[1]

    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return self.base.val ** self.exponent.val
        else:
            return '{}^({})'.format(self.base, self.exponent)

    def derivative(self, var: str):
        return \
            Add(
                Multiply(
                    self.exponent.derivative(var),
                    Ln(self.base),
                    self,
                ),
                Multiply(
                    self.exponent,
                    self.base.derivative(var),
                    Reciprocal(self.base),
                    self
                ),
            )


class Log(IFunc):
    def __init__(self, base: IFunc, antilog: IFunc):
        assert isinstance(base, IFunc) and isinstance(antilog, IFunc)
        self._vars: Tuple[IFunc, IFunc] = (base, antilog)

    @property
    def base(self):
        return self._vars[0]

    @property
    def antilog(self):
        return self._vars[1]

    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.vars):
            return self.base.val ** self.antilog.val
        else:
            return 'log({},{})'.format(self.base, self.antilog)

    def derivative(self, var: str):
        return \
            Multiply(
                Add(
                    Multiply(
                        self.antilog.derivative(var),
                        Ln(self.base),
                        Reciprocal(self.antilog)
                    ),
                    Negative(
                        Multiply(
                            self.base.derivative(var),
                            Ln(self.antilog),
                            Reciprocal(self.base)
                        )
                    )
                ),
                Power(
                    Ln(self.base),
                    Element(2)
                )
            )


class Ln(Log, UnaryMixIn):
    def __init__(self, antilog: IFunc):
        base = Element(math.e)
        super().__init__(base, antilog)


class MyFunc(object):
    traditional_functions: Dict[str, type] = {
        'log': Log,
    }
    __all: Dict[str, 'MyFunc'] = {}
    regex_heading = re.compile(
        r'^(?P<head>(?P<name>[a-zA-z]+)\((?P<parameters>(?:[a-z],)*[a-z])\)=)?(?P<expression>.*)$')
    regex_pre_naked_subs = re.compile(r'\w+(?=[,+*/^-])|(?:(?<=[,+*/^-])|^)\w+$')
    regex_outermost_func_name = re.compile(r'^(?P<name>[A-Za-z])\((?P<args>.+)\)$')

    def __init__(self, analytic: IFunc, parameters: Tuple[str, ...] = (), name: Optional[str] = None):
        self._name: Optional[str] = name
        self._analytic: IFunc = analytic
        self._parameters: Tuple[str, ...] = parameters
        self.__class__.__all.update({self._name: self})

    def __del__(self):
        try:
            self.__class__.__all.pop(self._name)
        except ValueError:
            pass

    @property
    def name(self):
        return self._name

    @property
    def analytic(self):
        return self._analytic

    @property
    def parameters(self):
        return self._parameters

    def __call__(self, *args: IFunc, **kwargs: IFunc) -> IFunc:
        n_args: Dict[str, IFunc] = dict(zip(self.parameters, args))
        n_args.update(kwargs)
        return self.analytic.apply(**n_args)

    @classmethod
    def __parentheses_ranges(cls, expr: str) -> List[Tuple[int, int]]:
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
        return wrapped_subs

    @classmethod
    def __subexpression_ranges(cls, expr: str) -> List[Tuple[int, int]]:
        wrapped_subs = cls.__parentheses_ranges(expr)
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

    @classmethod
    def __strip_redundant_parentheses(cls, expr):
        while len(cls.__subexpression_ranges(expr)) == 1 and expr[0] == '(' and expr[-1] == ')':
            # strip the outermost redundant parentheses ^(...)$
            expr = expr[1:len(expr) - 1]
        return expr

    @classmethod
    def __comma_split(cls, exprs: str) -> List[str]:
        commas = list(re.finditer(',', exprs))
        splits: List[int] = [0]
        parentheses_ranges = cls.__parentheses_ranges(exprs)
        args: List[str] = []
        if commas:
            for comma in commas:
                c_pos = comma.start()
                try:
                    for rg in parentheses_ranges:
                        if rg[0] < c_pos < rg[1]:
                            raise IndexError
                    splits.append(comma.start())
                except IndexError:
                    pass
            for i in range(len(splits) - 1):
                args.append(exprs[splits[i]: splits[i + 1]])
        else:
            args.append(exprs)
        return args

    @classmethod
    def __outermost_operator_from_expr(cls, expr: str) -> IFunc:
        name: Optional[str] = None
        splits: List[int] = [0]
        subexpression_ranges: List[Tuple[int, int]] = cls.__subexpression_ranges(expr)
        for opr in [{'+', '-'}, {'*', '/'}, {'^'}]:
            for rg in subexpression_ranges[:-1]:
                selected_opr = expr[rg[1]]
                if selected_opr in opr:
                    if name is None:
                        name = selected_opr
                    if expr[rg[1]] == name:
                        splits.append(rg[1])
            if len(splits) > 1:
                break
        splits.append(len(expr))
        return {'+': Add, '-': Add, '*': Multiply, '/': Multiply, '^': Power}[name] \
            (*(cls.from_expr(expr[splits[i]: splits[i + 1]]) for i in range(len(splits) - 1)))

    @classmethod
    def __expr_from_expr(cls, expr: str) -> IFunc:
        expr = cls.__strip_redundant_parentheses(expr)
        subexpression_ranges: List[Tuple[int, int]] = cls.__subexpression_ranges(expr)
        if len(subexpression_ranges) == 1:
            if expr[0] in {'+', '*'}:
                return cls.__expr_from_expr(expr[1:])
            elif expr[0] == '-':
                return Negative(cls.__expr_from_expr(expr[1:]))
            elif expr[0] == '/':
                return Reciprocal(cls.__expr_from_expr(expr[1:]))
            elif expr[-1] == ')':
                outermost_func: Optional[Match[str]] = cls.regex_outermost_func_name.match(expr)
                outermost_func_name: str = outermost_func.group('name')
                outermost_func_args: List[str] = cls.__comma_split(outermost_func.group('args'))
                if outermost_func_name in cls.traditional_functions:
                    return cls.traditional_functions[outermost_func_name](
                        *(cls.__expr_from_expr(arg)) for arg in outermost_func_args)
                elif outermost_func_name in cls.__all:
                    return cls.__all[outermost_func_name](
                        *(cls.__expr_from_expr(arg)) for arg in outermost_func_args)
                else:
                    raise ValueError
            else:
                return Element(expr)
        else:
            return cls.__outermost_operator_from_expr(expr)

    @classmethod
    def from_expr(cls, origin_str: str) -> 'MyFunc':
        head: Optional = cls.__class__.regex_heading.match(re.sub(r'\s', '', origin_str))
        return MyFunc(cls.__expr_from_expr(head.group('expression')),
                      tuple(head.group('parameters').split(',') if head is None else ['x']),
                      head.group('name')
                      )


if __name__ == '__main__':
    pass
