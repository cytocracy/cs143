class Main inherits IO {
   main(): Object {
    {
        out_string("Hello! Please Enter your name.\n");
        let name : String <- in_string() in {
            out_string("\nHello, ");
            out_string(name);
            out_string("!\nPlease think of a number between 1 and 10\n");
        };
        let x : Int <-1 in {
            out_string("Is your number ");
            out_int(x);
            out_string("?\n");
            while not (in_string() = "y") loop {

                x <- x + 1;
                out_string("Is your number ");
                out_int(x);
                out_string("?\n");
            } 
            pool;
            out_string("yay!");
        };

    }
   };
};
