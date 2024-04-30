def main():
INDENT     # Outer block
    x = 10

    if x > 5:
INDENT         # First nested block
        print("x is greater than 5")
       
        for i in range(x):
INDENT             # Second nested block
            if i % 2 == 0:
INDENT                 # Third nested block
                print(f"{i} is even")
DEDENT             else:
INDENT                 print(f"{i} is odd")
       
DEDENT DEDENT         def test():
INDENT             pass
   
DEDENT DEDENT     print("End of program")
