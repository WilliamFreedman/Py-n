import subprocess
import filecmp

def tester (input,  output):
    execut = []
    executable = "./pyn_runner.sh"
    execut.append(executable)
    execut.append(input)
    args = input
    result = subprocess.run(execut, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text = True, timeout = 5)

    result_str = result.stdout.strip()
    res_comp = open(output, 'r')
    res_comp_str = res_comp.read()

    if (result_str != res_comp_str):
        message = result_str
        return False, message
    else:
        return True, ""
    

def main ():
    test_files = [
        ("../../test/print_test.pyn", "../../test/print_test.txt"),
        ("../../test/test_basic_if.pyn", "../../test/test_basic_if.txt"),
        ("../../test/test_basic_if_else.pyn", "../../test/test_basic_if_else.txt"),
        ("../../test/test_func1.pyn", "../../test/test_func1.txt"),
        ("../../test/basic_loop_conditional.pyn", "../../test/basic_loop_conditional.txt"),
        ("../../test/bool_not_work.pyn", "../../test/bool_not_work.txt"),
        ("../../test/bool_test2.pyn", "../../test/bool_test2.txt"),
        ("../../test/bool_test3.pyn", "../../test/bool_test3.txt"),
        ("../../test/test_break_continue.pyn", "../../test/test_break_continue.txt"),
        ("../../test/test_if1.pyn", "../../test/test_if1.txt"),
        ("../../test/test_break_loop1.pyn", "../../test/test_break_loop1.txt"),
        ("../../test/test_recursion1.pyn", "../../test/test_recursion1.txt"),
        ("../../test/test_macro.pyn", "../../test/test_macro.txt"),
         ("../../test/test_ops.pyn", "../../test/test_ops.txt")
        
    ]
    tests_passed = 0
    for in_file, out_file in test_files:
        result, message = tester(in_file, out_file)
        if result:
            tests_passed += 1
            print ("Test Passes! for file: " + in_file)
        else:
            print("Test failed for: " + in_file)
            print(message)
        print("")
    print(str(tests_passed) + " out of " + str(len(test_files)) + " tests passed.")

if __name__ == "__main__":
    main()