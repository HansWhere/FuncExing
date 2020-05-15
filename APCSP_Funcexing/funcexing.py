# Pan Lin
# Mr. Thierfelder
# AP Computer Science Principles
# 9 April 2020
# (independent development by Pan Lin)
import re
import math
from abc import ABCMeta, abstractmethod, ABC
from functools import reduce
from typing import List, Optional, Union, Tuple, Dict, Match


class IFunc(metaclass=ABCMeta):
    # an interface of mathematical function types
    def __init__(self, *args: 'IFunc'):
        assert all(isinstance(arg, IFunc) for arg in args)
        # the arguments of the function
        self._args: Tuple[IFunc, ...] = args

    @property
    def args(self) -> Tuple['IFunc', ...]:
        # read-only
        return self._args

    @property
    @abstractmethod
    def val(self) -> Union[str, int, float]:
        # implement this method to set the value of the function
        pass

    @abstractmethod
    def derivative(self, var: str) -> 'IFunc':
        # "var" is the independent variable of the derivative
        # implement this method to set the derivative of the function
        # need to consider the chain rule
        pass

    def apply(self, **kwargs: 'IFunc') -> 'IFunc':
        # replace the subexpressions of the function
        new_func: IFunc = self.__new__(self.__class__)
        new_func.__init__(*(var.apply(**kwargs) for var in self.args))
        return new_func

    def __call__(self, **kwargs: 'IFunc') -> 'IFunc':
        # syntactic sugar of "apply"
        return self.apply(**kwargs)

    def __eq__(self, other: 'IFunc') -> bool:
        # It is almost impossible to check whether two elementary function are equal
        # So this only represents that the two functions are formally equal
        return self.__class__ == other.__class__ and self.args == other.args

    def __str__(self) -> str:
        return '({})'.format(str(self.val))


class IUnaryFunc(IFunc, ABC):
    # the interface of the functions which has one and only one argument
    def __init__(self, arg: 'IFunc'):
        assert isinstance(arg, IFunc)
        self._args: Tuple[IFunc, ...] = (arg,)

    @property
    def var(self) -> IFunc:
        return self.args[0]


class Element(IFunc):
    def __init__(self, val: Union[str, int, float]):
        # try to convert the argument to a numerical value (int or float)
        # If we can't, then just regard it as an independent variable
        if isinstance(val, str) and val.isnumeric():
            val = int(val)
        else:
            try:
                val = float(val)
            except ValueError:
                pass
        self._val: Union[str, int, float] = val
        self._args = None

    @property
    def val(self) -> Union[str, int, float]:
        return self._val

    def apply(self, **kwargs: 'IFunc') -> 'IFunc':
        return Element(kwargs[self.val] if self.val in kwargs else self.val)

    def derivative(self, var: str) -> 'IFunc':
        # dx/dx = 1  the derivative of 'x' is 1
        # dC/dx = 0  the derivative of a constant is 0
        return Element(1 if self.val == var else 0)

    def __eq__(self, other: 'IFunc') -> bool:
        return self.val == other.val

    def __str__(self) -> str:
        return str(self.val)


class Add(IFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return sum(var.val for var in self.args)
        else:
            # if the value cannot be calculated, just return the whole expression
            return '+'.join(str(var.val) for var in self.args)

    def derivative(self, var: str) -> IFunc:
        # (d/dx)(y_1 + y_2 + ... + y_n) = d(y_1)/dx + d(y_2)/dx + ... + d(y_n)/dx
        return Add(*(v.derivative(var) for v in self.args))

    def __eq__(self, other: 'IFunc') -> bool:
        return self.__class__ == other.__class__ and \
               all(var in other.args for var in self.args) and \
               all(var in self.args for var in other.args)


class Negative(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return -self.var.val
        else:
            return '(-{})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        # d(-y)/dx = -(dy/dx)
        return Negative(self.var.derivative(var))


class Multiply(IFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return reduce(lambda x, y: x * y, (var.val for var in self.args))
        else:
            return '*'.join('({})'.format(str(var.val)) for var in self.args)

    def derivative(self, var: str) -> IFunc:
        # product rule
        # (d/dx)(y_1 * y_2) = (y_1)'*y_2 + y_1*(y_2)'
        # (d/dx)(y_1 * y_2 * ... * y_n) = (y_1)'*y_2*...*y_n + y_1*(y_2)'*...*y_n + ... + y_1*y_2*...*(y_n)'
        return \
            Add(*(
                    Multiply(*(
                            self.args[:n] + (self.args[n].derivative(var),) + self.args[n + 1:]
                    )) for n in range(len(self.args)))
                )

    def __eq__(self, other: 'IFunc') -> bool:
        return self.__class__ == other.__class__ and \
               all(var in other.args for var in self.args) and \
               all(var in self.args for var in other.args)


class Reciprocal(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return 1 / self.var.val
        else:
            return '(1/({}))'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        # d(1/y)/x = -(dy/dx)/y^2
        return Negative(Multiply(self.var.derivative(var), Reciprocal(Power(self.var, Element(2)))))


class Power(IFunc):
    def __init__(self, base: IFunc, *nested_exponents: IFunc):
        assert isinstance(base, IFunc) and all(isinstance(index, IFunc) for index in nested_exponents)
        if len(nested_exponents) > 1:
            self._args: Tuple[IFunc, IFunc] = (base, Power(*nested_exponents))
        elif len(nested_exponents) == 1:
            self._args: Tuple[IFunc, IFunc] = (base, nested_exponents[0])

    @property
    def base(self) -> IFunc:
        return self._args[0]

    @property
    def exponent(self) -> IFunc:
        return self._args[1]

    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return self.base.val ** self.exponent.val
        else:
            return '{}^({})'.format(self.base, self.exponent)

    def derivative(self, var: str) -> IFunc:
        # (f(x)^g(x))' = f(x)^g(x)*(g'(x)*ln(f(x))+g(x)*f'(x)/f(x))
        return \
            Add(
                Multiply(
                    self.exponent.derivative(var),
                    Ln(self.base),
                    self
                ),
                Multiply(
                    self.exponent,
                    self.base.derivative(var),
                    Reciprocal(self.base),
                    self
                ),
            )


class Sqrt(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.sqrt(self.var.val)
        else:
            return 'sqrt({})'.format(self.var.val)

    def derivative(self, var: str) -> IFunc:
        # (sqrt(f(x)))' = f'(x)/(2*sqrt(f(x)))
        return \
            Multiply(
                self.var.derivative(var),
                Reciprocal(
                    Multiply(
                        Element(2),
                        Sqrt(self.var)
                    )
                )
            )


class Log(IFunc):
    def __init__(self, base: IFunc, antilog: IFunc):
        assert isinstance(base, IFunc) and isinstance(antilog, IFunc)
        self._args: Tuple[IFunc, IFunc] = (base, antilog)

    @property
    def base(self) -> IFunc:
        return self._args[0]

    @property
    def antilog(self) -> IFunc:
        return self._args[1]

    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return self.base.val ** self.antilog.val
        else:
            return 'log({},{})'.format(self.base, self.antilog)

    def derivative(self, var: str) -> IFunc:
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


class Ln(Log, IUnaryFunc):
    def __init__(self, antilog: IFunc):
        assert isinstance(antilog, IFunc)
        self._args = (antilog,)

    @property
    def base(self) -> IFunc:
        return Element(math.e)

    @property
    def antilog(self) -> IFunc:
        return self.var

    @property
    def val(self) -> Union[str, int, float]:
        if isinstance(self.var.val, (int, float)):
            return math.log(self.antilog.val)
        else:
            return 'ln({})'.format(self.antilog)

    def derivative(self, var: str) -> IFunc:
        return Multiply(self.antilog.derivative(var), Reciprocal(self.antilog))


class Sin(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.sin(self.var.val)
        else:
            return 'sin({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Multiply(Cos(self.var), self.var.derivative(var))


class Cos(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.cos(self.var.val)
        else:
            return 'cos({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Negative(Multiply(Sin(self.var), self.var.derivative(var)))


class Tan(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.tan(self.var.val)
        else:
            return 'tan({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Multiply(Power(Sec(self.var), Element(2)), self.var.derivative(var))


class Cot(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return 1 / math.tan(self.var.val)
        else:
            return 'cot({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Negative(Multiply(Power(Csc(self.var), Element(2)), self.var.derivative(var)))


class Sec(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return 1 / math.cos(self.var.val)
        else:
            return 'sec({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Multiply(Sec(self.var), Tan(self.var), self.var.derivative(var))


class Csc(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return 1 / math.sin(self.var.val)
        else:
            return 'tan({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return Negative(Multiply(Csc(self.var), Cot(self.var), self.var.derivative(var)))


class Arcsin(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.asin(self.var.val)
        else:
            return 'arcsin({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return \
            Multiply(
                self.var.derivative(var),
                Reciprocal(
                    Sqrt(Add(Element(1), Negative(Power(self.var, Element(2)))))
                )
            )


class Arccos(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.acos(self.var.val)
        else:
            return 'arccos({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return \
            Negative(
                Multiply(
                    self.var.derivative(var),
                    Reciprocal(
                        Sqrt(Add(Element(1), Negative(Power(self.var, Element(2)))))
                    )
                )
            )


class Arctan(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.atan(self.var.val)
        else:
            return 'arctan({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return \
            Multiply(
                self.var.derivative(var),
                Reciprocal(
                    Add(Element(1), Power(self.var, Element(2)))
                )
            )


class Arccot(IUnaryFunc):
    @property
    def val(self) -> Union[str, int, float]:
        if all(isinstance(var.val, (int, float)) for var in self.args):
            return math.pi / 2 - math.atan(self.var.val)
        else:
            return 'arccot({})'.format(str(self.var.val))

    def derivative(self, var: str) -> IFunc:
        return \
            Negative(
                Multiply(
                    self.var.derivative(var),
                    Reciprocal(
                        Add(Element(1), Power(self.var, Element(2)))
                    )
                )
            )


class MyFunc(object):
    # the class of customized functions
    # Each instance is a customized function
    traditional_functions: Dict[str, type] = {
        'sqrt': Sqrt,
        'log': Log,
        'ln': Ln,
        'sin': Sin,
        'cos': Cos,
        'tan': Tan,
        'cot': Cot,
        'sec': Sec,
        'csc': Csc,
        'arcsin': Arcsin,
        'arccos': Arccos,
        'arctan': Arctan,
        'arccot': Arccot
    }
    __all: Dict[str, 'MyFunc'] = {}  # The dictionary of customized functions
    regex_heading = re.compile(  # the regular expression to separate the head and body
        r'^(?P<head>(?P<name>[a-zA-z]+)\((?P<parameters>(?:[a-z],)*[a-z])\)=)?(?P<expression>.*)$')
    # to catch some substrings which might be subexpressions
    regex_pre_naked_subs = re.compile(r'\w+(?=[,+*/^-])|(?:(?<=[,+*/^-])|^)\w+$')
    # for the expressions shaped like "f(arg_1, arg_2, ...)", catch 'f' as 'name' and 'arg_1, arg_2, ...' as 'args'
    regex_outermost_func_name = re.compile(r'^(?P<name>[A-Za-z]+)\((?P<args>.+)\)$')

    def __init__(self, analytic: IFunc, parameters: Tuple[str, ...] = (), name: Optional[str] = None):
        self._name: Optional[str] = name
        self._analytic: IFunc = analytic
        self._parameters: Tuple[str, ...] = parameters
        self.__class__.__all.update({self._name: self})

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
        # substitute the arguments into the variables
        # if you do not want to change the expression of your customized function, just input nothing
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

    @classmethod
    def __strip_redundant_parentheses(cls, expr):
        while len(cls.__subexpression_ranges(expr)) == 1 and expr[0] == '(' and expr[-1] == ')':
            # strip the outermost redundant parentheses ^(...)$
            expr = expr[1:len(expr) - 1]
        return expr

    @classmethod
    def __comma_split(cls, exprs: str) -> List[str]:
        # one of the two subroutines of the selected method
        commas = list(re.finditer(',', exprs))
        splits: List[int] = [0]
        stats: int = 0
        parentheses_ranges: List[Union[Tuple[int, int], Tuple[int]]] = []
        for index, char in enumerate(exprs):
            assert stats >= 0
            if char == "(":
                if not parentheses_ranges or len(parentheses_ranges[-1]) == 2:
                    parentheses_ranges.append((index,))
                stats += 1
            elif char == ")":
                stats -= 1
                if stats == 0 and len(parentheses_ranges[-1]) == 1:
                    parentheses_ranges[-1] += (index + 1,)
        assert not parentheses_ranges or len(parentheses_ranges[-1]) == 2
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
        # one of the two subroutines of the selected method
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
        return {'+': Add, '-': Add, '*': Multiply, '/': Multiply, '^': Power}[name](
            *(cls.__expr_from_expr(expr[splits[i]: splits[i + 1]]) for i in range(len(splits) - 1)))

    @classmethod
    def __expr_from_expr(cls, expr: str) -> IFunc:
        # selected method in 2b
        expr = cls.__strip_redundant_parentheses(expr)
        subexpression_ranges: List[Tuple[int, int]] = cls.__subexpression_ranges(expr)
        if len(subexpression_ranges) == 1:
            if expr[0] in {'+', '*', '^'}:
                return cls.__expr_from_expr(expr[1:])
            elif expr[0] == '-':
                return Negative(cls.__expr_from_expr(expr[1:]))
            elif expr[0] == '/':
                return Reciprocal(cls.__expr_from_expr(expr[1:]))
            elif expr[-1] == ')':
                outermost_func: Optional[Match[str]] = cls.regex_outermost_func_name.match(expr)
                assert outermost_func is not None
                outermost_func_name: str = outermost_func.group('name')
                outermost_func_args: List[str] = cls.__comma_split(outermost_func.group('args'))
                if outermost_func_name in cls.traditional_functions:
                    return cls.traditional_functions[outermost_func_name](
                        *(cls.__expr_from_expr(arg) for arg in outermost_func_args))
                elif outermost_func_name in cls.__all:
                    return cls.__all[outermost_func_name](
                        *(cls.__expr_from_expr(arg) for arg in outermost_func_args))
                else:
                    raise ValueError
            else:
                return Element(expr)
        else:
            return cls.__outermost_operator_from_expr(expr)

    @classmethod
    def from_expr(cls, origin_str: str) -> 'MyFunc':
        head: Optional = cls.regex_heading.match(re.sub(r'\s', '', origin_str))
        analytic = cls.__expr_from_expr(head.group('expression'))
        parameters = tuple(head.group('parameters').split(',') if head is None else ['x'])
        return MyFunc(analytic,
                      parameters,
                      head.group('name')
                      )

# The user can calculate the derivative of their customized function
# However, I still cannot simplify the function structure :(


if __name__ == '__main__':
    eg_f = 'f(x)=cos(e^x-x^2)*ln(x/2)'
    eg_c = 'c(x)=(cos((e^x)-(x^2))*(ln(x/2)))+x*2^x'
    print(MyFunc.from_expr(eg_f)().derivative('x'))
    print(eg_c)
    print(MyFunc.from_expr(eg_c)('x'))
    print(MyFunc.from_expr(eg_c)(10))
