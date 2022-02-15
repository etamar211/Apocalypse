import subprocess
from tkinter import *
from PIL import ImageTk, Image
import time
import ast

#ofssets for the position of the pieces relative to the board.
DICT_Y_OFFSET = {
    1 : 10,
    2 : 160,
    3 : 305,
    4 : 450,
    5 : 595
}

DICT_X_OFFSET = {
    1 : 140,
    2 : 290,
    3 : 440,
    4 : 590,
    5 : 740
}

board = []

waitingForInput = False

p = subprocess.Popen(['swipl', '-q', '-f', 'Apocalypse.pl', '-g', 'apocalypse'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, universal_newlines=True)


def input_handle(window, frm_boardFrame, ent_userInputField, txt_textBox):
    global p
    global waitingForInput
        
    userInput = ent_userInputField.get()
    ent_userInputField.delete(0, END)
    
    if userInput == "restart":
        p.terminate()
        p = subprocess.Popen(['swipl', '-q', '-f', 'Apocalypse.pl', '-g', 'apocalypse'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, universal_newlines=True)
                
        #don't print rules again but get them from prolog
        output = p.stdout.readline()
        p.stdout.flush()
        while output != "Choose level:\n":
            output = p.stdout.readline()
            p.stdout.flush()
        
        #print level choosing
        outputWords = output.split(" ")
        while output != "FP\n":
            for word in outputWords:
                txt_textBox.insert("end", word + ' ')
                time.sleep(0.15)
                window.update()
            output = p.stdout.readline()
            p.stdout.flush()
            outputWords = output.split(" ")
            txt_textBox.see("end")
        
        waitingForInput = True
        return
    
    if userInput == "quit":
        window.destroy()
        p.terminate()
        return
        
    if waitingForInput == False:
        ent_userInputField.delete(0, END)
        return
    
    
    #waitingForInput == True. send input to prolog (text handling will be done by prolog)
    txt_textBox.insert("end", "-" + userInput + "\n")
    p.stdin.flush()
    p.stdin.write(userInput + "\n")
    p.stdin.flush()
    window.update()
    waitingForInput = False
    print_prolog_output(window, frm_boardFrame, txt_textBox)

def print_prolog_output(window, frm_boardFrame, txt_textBox):
    global waitingForInput
    
    output = p.stdout.readline()
    p.stdout.flush()
    if len(output):
        if output.endswith(']]\n'):
            output = output.replace('[[', '[["').replace(']]', '"]]').replace(',', '","').replace(']","[', '"],["')
            set_board(frm_boardFrame, output)
            output = p.stdout.readline()
            p.stdout.flush()
    outputWords = output.split(" ")
    
    while output != "FP\n":
        for word in outputWords:
            txt_textBox.insert("end", word + ' ')
            #time.sleep(0.15)
            window.update()
        output = p.stdout.readline()
        p.stdout.flush()
        if len(output):
            if output.endswith(']]\n'):
                output = output.replace('[[', '[["').replace(']]', '"]]').replace(',', '","').replace(']","[', '"],["')
                set_board(frm_boardFrame, output)
                output = p.stdout.readline()
                p.stdout.flush()
        outputWords = output.split(" ")
        txt_textBox.see("end")
    
    waitingForInput = True
    
def set_board(frm_boardFrame, boardToSet):
    global board
    
    for piece in board:
        piece.destroy()
    
    newBoard = []
    boardToSetList = ast.literal_eval(boardToSet)

    i = 1
    for line in boardToSetList:
        j = 1
        for square in line:
            if square != '_':
                lbl = Label(frm_boardFrame, image=DICT_IMAGES[square])
                lbl.pack()
                lbl.place(x = DICT_X_OFFSET[j], y = DICT_Y_OFFSET[i])
                newBoard.append(lbl)
            j += 1
        i += 1
    board = newBoard

#set window
window = Tk()
window.resizable(width= True, height = True)

#set text frame for input and output
frm_textFrame = Frame()
txt_textBox = Text(master = frm_textFrame)
txt_textBox.pack()
lbl_input = Label(text= "input:", master=frm_textFrame)
lbl_input.pack(side=LEFT)
ent_userInputField = Entry(master=frm_textFrame, width = 100)
ent_userInputField.pack()
btn_enter = Button(master=frm_textFrame, text="Enter", command= lambda: input_handle(window, frm_boardFrame, ent_userInputField, txt_textBox))
btn_enter.pack()
frm_textFrame.pack(side=RIGHT)

#set board frame and import images
frm_boardFrame = Frame()
img_board = ImageTk.PhotoImage(Image.open("Board.png"))
lbl_board = Label(frm_boardFrame, image=img_board)
lbl_board.pack()
img_whitePawn = ImageTk.PhotoImage(Image.open("WhitePawn.png").resize((122, 129)))
img_blackPawn = ImageTk.PhotoImage(Image.open("BlackPawn.png").resize((120, 136)))
img_whiteKnight = ImageTk.PhotoImage(Image.open("WhiteKnight.png").resize((122, 129)))
img_blackKnight = ImageTk.PhotoImage(Image.open("BlackKnight.png").resize((120, 136)))

DICT_IMAGES = {
    'BP' : img_blackPawn,
    'WP' : img_whitePawn,
    'BK' : img_blackKnight,
    'WK' : img_whiteKnight
}

frm_boardFrame.pack(side=LEFT)

print_prolog_output(window, frm_boardFrame, txt_textBox)

window.mainloop()
