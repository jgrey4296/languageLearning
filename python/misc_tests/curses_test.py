import logging
import curses
from curses import wrapper
from curses.textpad import Textbox, rectangle
import IPython

def split_into_columns(scr,n):
    width = int(curses.COLS / n)
    height = curses.LINES
    logging.info("Width is: {}. Height is : {}".format(width,height))
    columns = []
    for i in range(n):
        pos_x = width * i
        logging.info("Creating {} at y:{}, x: {}".format(i,0,pos_x))
        col = scr.subwin(height,width,0,pos_x)
        col.box()
        columns.append(col)

    return columns


def main(stdscr):
    stdscr.clear()
    columns = split_into_columns(stdscr,3)
    shouldExit = False

    box = Textbox(columns[0])
    rect = rectangle(columns[0],5,5,0,0)

    while not shouldExit:
        box.edit()
        k = stdscr.getkey()

        if k == "q":
            shouldExit = True

def update(columns):
    [col.refresh() for col in columns]

if __name__ == "__main__":
    logging.basicConfig(filename="curses.log",level=logging.DEBUG,filemode='w')
    wrapper(main)
