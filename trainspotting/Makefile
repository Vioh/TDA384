all: src/*.java
	javac -sourcepath src -d bin src/Main.java

run:
	java -cp bin Main Lab1.map 5 15

tsim:
	tsim Lab1.map

clean:
	rm -rf bin/*
