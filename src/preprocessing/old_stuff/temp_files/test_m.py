def main():
    # Outer block
    x = 10

    if x > 5:
        # First nested block
        print("x is greater than 5")
       
        for i in range(x):
            # Second nested block
            if i % 2 == 0:
                # Third nested block
                print(f"{i} is even")
            else:
                print(f"{i} is odd")
       
        def test():
            pass
   
    print("End of program")
