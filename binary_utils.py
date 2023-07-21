from dataclasses import dataclass


@dataclass
class BinaryAddSubResult:
    lower_bits: list[bool]
    carry: bool


def add_bits(bit1: bool, bit2: bool, carry: bool) -> BinaryAddSubResult:
    result = BinaryAddSubResult([], False)
    bits = [bit1, bit2, carry]
    if bits.count(True) == 0:
        result.lower_bits = [False]
    elif bits.count(True) == 1:
        result.lower_bits = [True]
    elif bits.count(True) == 2:
        result.lower_bits = [False]
        result.carry = True
    elif bits.count(True) == 3:
        result.lower_bits = [True]
        result.carry = True
    return result


def bin_nums_as_eq_len(num1: list[bool], num2: list[bool]) -> (list[bool], list[bool]):
    """Makes 2 binary numbers equal length by adding 0s to the start of the smaller one.
    Doesn't modify the arguments.

    Example:
    ``bin_nums_as_eq_len([True], [False, True, True]) -> ([False, False, True], [False, True, True])``
    """
    max_len = max(len(num1), len(num2))
    # this will only pad the shorter one, because e.g. [False] * -2 == []
    padded_num1 = [False] * (max_len - len(num1)) + num1
    padded_num2 = [False] * (max_len - len(num2)) + num2
    return padded_num1, padded_num2


def add_binary(num1: list[bool], num2: list[bool], carry: bool) -> BinaryAddSubResult:
    num1, num2 = bin_nums_as_eq_len(num1, num2)
    answer = []
    for bit1, bit2 in reversed(list(zip(num1, num2))):
        bits_added = add_bits(bit1, bit2, carry)
        carry = bits_added.carry
        answer.insert(0, bits_added.lower_bits[0])
    return BinaryAddSubResult(answer, carry)


def sub_binary(num1: list[bool], num2: list[bool], carry: bool) -> BinaryAddSubResult:
    return add_binary(num1, [not bit for bit in num2], not carry)


def binary_to_string(num: list[bool]) -> str:
    out = "".join(["1" if bit else "0" for bit in num])
    if out == "":
        out = "0"
    return out


def string_to_binary(s: str) -> list[bool]:
    out = []
    for c in s:
        if c == "1":
            out.append(True)
        elif c == "0":
            out.append(False)
        else:
            raise Exception("string can only have 1s and 0s, found", c)
    return out


def binary_to_int(num: list[bool]):
    return int(binary_to_string(num), 2)


def int_to_binary(num: int, n_digits: int) -> list[bool]:
    assert num >= 0, f"Can't convert negative number ({num}) to binary"
    res = string_to_binary(bin(num)[2:])
    assert len(res) <= n_digits, f"Result exceeds {n_digits} digits"
    while len(res) != n_digits:
        res.insert(0, False)
    return res
