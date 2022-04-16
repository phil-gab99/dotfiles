import contextlib
import os
import re
import subprocess
import numpy as np

@contextlib.contextmanager
def directory(name):
    ret = os.getcwd()
    os.chdir(name)
    yield None
    os.chdir(ret)

def shell(cmd):
    try:
        program_output = subprocess.check_output(cmd, shell=True, universal_newlines=True, stderr=subprocess.STDOUT)
    except Exception as e:
        program_output = e.output
    print(program_output)
    return program_output.strip()

valgrind_output = ""
def run(input_cmd, in_ours=False):
    global valgrind_output
    def reset_bs():
        with open("bs.txt", "w") as f:
            for _ in range(65536):
                f.write('a')
    reset_bs()

    executable = "TP3_sol" if in_ours else "TP3"

    dir = shell('echo $PWD')
    pardir = "/".join(dir.split("/")[:-1])

    shell_cmd = f"timeout 30 valgrind --leak-check=full \
        --leak-resolution=med \
        --trace-children=no \
        --track-origins=yes \
        --vgdb=no \
        --log-file=\"valgrind.log\" \
        ./{executable} {dir}/bs.txt <{pardir}/cmds/{input_cmd} > results.txt && cat report.txt"

    program_output = shell(shell_cmd)

    try:
        with open("./valgrind.log", "r") as f:
            valgrind_output += f.read()
    except:
        valgrind_output += "definitely lost: 9\n\n"

    program_output, vmm_report = program_output.split("DICO_SEP")

    def str2num(str): return float(str.replace("%", "")) if "%" in str else int(str)
    vmm_report = {x.split(":")[0]: str2num(x.split(":")[1]) for x in vmm_report.strip().split("\n")}

    class Call:
        def __init__(self, is_reading, byte, addr, p, o, f, pa):
            self.is_reading = is_reading
            self.byte = byte
            self.addr = addr
            self.p = p
            self.o = o
            self.f = f
            self.pa = pa
        def __str__(self): return f"{self.is_reading}, {self.byte}, {self.addr}, {self.p}, {self.o}, {self.f}, {self.pa}"
        def __repr__(self): return self.__str__().replace(", ", "_")

    def line2call(line):
        line = line.strip()
        line = line.replace("[", " ").replace("]", " ").replace("@", " ").replace(",", "").replace(":", "")
        line = re.sub(r"\s+", " ", line)
        line = line.split(" ")

        mode = line[0] == "reading"
        byte = line[1]
        addr = line[2]

        def aftereq(str): return str.split("=")[-1]
        p = aftereq(line[3])
        o = aftereq(line[4])
        f = aftereq(line[5])
        pa = aftereq(line[6])

        return Call(mode, byte, addr, p, o, f, pa)

    calls = [line2call(x) for x in program_output.strip().split("\n")]

    def get_bs():
        with open("bs.txt", "r") as f:
            r = f.read()
        return r.strip()

    return calls, vmm_report, get_bs()

#with directory("tests_build"):
#    run("__1__.in")

with open("./valgrind.log", "w") as f:  # reset
    f.write("")

def executable(func):
    def wrapper():
        with directory("tests_build"):
            func()
    return wrapper

succ = 0
def good(pts):
    global succ
    succ += pts
    print(f"PTS:{pts}")
    return pts

print("GRADE IS OUT OF 120!!!!!!!!")

@executable
def _1():
    print("Is tlb test_inout good?")
    _, report, _ = run("../../src/command_tlb.in")
    if report["tlbchanges"] <= 8:
        print(f"\tIt isn't. Only {report['tlbchanges']} changes for tlb size 8")
    else:
        print("\tIt is.")
        return good(10)
    return 0

@executable
def _2():
    print("Is pt test_inout good?")
    _, report, _ = run("../../src/command_pt.in")
    if report["tlbchanges"] <= 32:
        print(f"\tIt isn't. Only {report['ptchanges']} changes for tlb size 32")
    else:
        print("\tIt is.")
        return good(10)
    return 0

print("Tests 1 and 2 go together!")

def call_baselines(file, for_tlb=True):
    pages = []
    with open(file, "r") as f:
        for line in f.readlines():
            line = re.sub("\D", "", line)
            pages.append(int(line))
    size = 8 if for_tlb else 32

    def __fifo():
        faults = 0
        mem = []
        for p in pages:
            if len(mem) < size:
                mem.append(p)
                faults += 1
            else:
                if p in mem:
                    continue

                mem.pop(0)
                mem.append(p)
                faults += 1
        return faults

    # Least Recently Used Page Replacement Algorithm
    def __lru():
        faults = 0
        mem = []
        for p in pages:
            if len(mem) < size:
                mem.append(p)
                faults += 1
            else:
                if p in mem:
                    mem.remove(p)
                else:
                    mem.pop(0)
                    faults += 1

                mem.append(p)
        return faults

    best = __lru()
    mid = __fifo()
    worst = len(pages)
    return best, mid, worst

def scale(real, best, mid, worst):
    if real <= best:
        return 1

    if real <= mid:
        real -= mid
        best -= mid

        return abs((real / best) / 4 + 0.75)

    if real <= worst:
        real -= mid
        worst -= mid

        return abs((1 - real / worst) * 0.75)

    return 0

@executable
def _3():
    print("How's your tlb replacement algorithm?")

    tests = []
    nb_tests = 1
    for i in range(nb_tests):
        best, mid, worst = call_baselines(f"../cmds/__tlb{i}__.in", True)
        _, report, _ = run(f"__tlb{i}__.in")
        tlbmisses = report["tlbmisses"]

        if tlbmisses <= best / 2:
            tests.append(0)
        else:
            tests.append(scale(tlbmisses, best, mid, worst))


    return good(10*np.mean(np.array(tests)))


@executable
def _4():
    print("How's your pt replacement algorithm?")

    tests = []
    nb_tests = 1
    for i in range(nb_tests):
        best, mid, worst = call_baselines(f"../cmds/__pt{i}__.in", False)
        _, report, _ = run(f"__pt{i}__.in")
        tlbmisses = report["pagefaults"]

        if tlbmisses <= best / 2:
            tests.append(0)
        else:
            tests.append(scale(tlbmisses, best, mid, worst))

    return good(10*np.mean(np.array(tests)))

print("Tests 1 and 4 go together")

def _5():
    print("And now, for the vmm implementation comparison")

    nb_tests = len(os.listdir("cmds"))
    successes = 0

    for file in os.listdir("cmds"):
        with directory("tests_build"):
            t_calls, t_report, t_bs = run(file, False)
            o_calls, o_report, o_bs = run(file, True)
            if t_bs == o_bs:
                successes += 1

    return good(successes/nb_tests * 20)

_1()
_2()
_3()
_4()
_5()

# valgrind
def pts_lost_for_mem_leaks(val):
    if len(re.findall(r"(definitely lost|indirectly lost): [1-9]", val)) == 0:
        return 0
    return 15

def pts_lost_for_invalids(val):
    return min(val.count("invalid read of size") + val.count("invalid write of size"), 5)


try:
    pts_lost = -(pts_lost_for_mem_leaks(valgrind_output) + pts_lost_for_invalids(valgrind_output)) / 100
except Exception:
    pts_lost = -0.2
pts_lost = pts_lost * 120
print(f"Points lost with Valgrind: {pts_lost}")

with directory("../test_check"):
    out = shell("make alltests")
    out2 = re.search(r"SUBGRADE:\{(.+)\}", out).group(1)
    subgrade = float(out2)*3*20

print(f"SUCCESSES:{succ}")
print(f"GRADE:{{{(subgrade + succ + pts_lost)/120}}}")