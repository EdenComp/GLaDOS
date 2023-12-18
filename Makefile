SRC		=	app/Main.hs 		\

NAME	=	glados

STACK	=	stack

all:        $(NAME)

$(NAME):
	$(STACK) build
	find . -name $(NAME) -type f -exec mv {} . \; -quit

clean:
	$(RM) $(SRC:.hs=.hi)
	$(RM) $(SRC:.hs=.o)

fclean: clean
	$(RM) $(NAME)
	$(RM) -r .stack-work

re: fclean all

tests_run:
	$(STACK) test --coverage
	$(STACK) hpc report --all --destdir test/coverage

func_tests:
	@$(MAKE) re 2> /dev/null
	@python3 -m pip install termcolor > /dev/null
	python3 test/functional/run.py -adc


.PHONY: all clean fclean re tests_run
