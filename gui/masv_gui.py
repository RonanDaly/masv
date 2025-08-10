import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QPushButton, QFileDialog

# Subclass QMainWindow to customize your application's main window
class MainMasvWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("MASV GUI")

        # Button to select MASV file
        select_file_button = QPushButton("Select MASV file")
        select_file_button.setCheckable(True)
        select_file_button.clicked.connect(self.select_file_click)

        # Table to display data

        # Set the central widget of the Window.
        self.setCentralWidget(select_file_button)
    
    def select_file_click(self):
            masv_file = QFileDialog.getOpenFileName(self, self.tr("Select MASV file"), "", self.tr("Masv file (*.tsv)"))
            print(masv_file)


app = QApplication(sys.argv)

window = MainMasvWindow()
window.show()

# Start the event loop.
app.exec()