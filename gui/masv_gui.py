import sys
import pandas as pd

from PyQt6.QtCore import QAbstractTableModel, Qt
from PyQt6.QtGui import QBrush, QColor
from PyQt6.QtWidgets import QApplication, QMainWindow, QPushButton, QFileDialog, QTableView, QWidget

# Model to contain masv data
class MasvTableModel(QAbstractTableModel):
    def __init__(self, data, col_highlight = None, row_highlight = None):
        super().__init__()
        self._data = data

        self.col_highlight = col_highlight  
        self.row_highlight = row_highlight
        self.colors = dict()

    def data(self, index, role, col_highlight = None, row_highlight = None):
        if role == Qt.ItemDataRole.DisplayRole:
            return self._data[index.row()][index.column()]
        elif role == Qt.ItemDataRole.BackgroundRole and index.column() == self.col_highlight:
            # See below for the data structure.
            if index.row() == self.row_highlight or self.row_highlight == None:
                return QColor('red')
        elif role == Qt.ItemDataRole.BackgroundRole and index.row() == self.row_highlight:
            # See below for the data structure.
            if index.column() == self.col_highlight or self.col_highlight == None:
                return QColor('red')

    # returns num of rows
    def rowCount(self, index):
        return len(self._data)
    
    # returns num of columns (assumes all rows of equal length)
    def columnCount(self, index):
        return len(self._data[0])
    

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
        self.table = QTableView()

        # Set the central widget of the Window.
        self.setCentralWidget(select_file_button)
    
    def select_file_click(self):
            masv_file_select = QFileDialog.getOpenFileName(self, self.tr("Select MASV file"), "", self.tr("Masv file (*.tsv)"))
            filepath = masv_file_select[0]

            masv_df = pd.read_csv(filepath, sep='\t')

            self.model = MasvTableModel(masv_df.values.tolist(), col_highlight = 2, row_highlight = 2)
            self.table.setModel(self.model)
            self.setCentralWidget(self.table)
            #print(masv_df)
                 


app = QApplication(sys.argv)

window = MainMasvWindow()
window.show()

# Start the event loop.
app.exec()