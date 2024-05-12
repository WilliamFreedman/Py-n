import subprocess
import filecmp

def tester (input,  output):
    execut = []
    executable = "../src/parsing/pyn_runner.sh"
    execut.append(executable)
    execut.append(input)
    args = input
    result = subprocess.run(execut, text = True, capture_output = True, timeout = 5)

    result_str = result.stdout.strip()
    res_comp = open(output, 'r')
    res_comp_str = res_comp.read()

    print(result_str)
    if (result_str != res_comp_str):
        return False, "The outputs are different"
    else:
        return True, ""
    

def main ():
    test_files = [
        ("print_test.pyn", "print_test.txt")
    ]

    for in_file, out_file in test_files:
        result, message = tester(in_file, out_file)
        if result:
            print ("Test Passes! for file: " + in_file)
        else:
            print("Test failed for: " + in_file)
            print(message)

if __name__ == "__main__":
    main()