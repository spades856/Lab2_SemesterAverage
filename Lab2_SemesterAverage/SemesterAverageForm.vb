Option Strict On

' Name: Jacob De Lio
' Date last modified: June 13th, 2020
'Description: A form that takes users grades and outputs a letter grade as well as their semester average.

Public Class frmSemesterAverage

#Region "Constants and Variables"
    ' Constants for computing the grade ranges
    Private Const MIN_GRADE As Integer = 0
    Private Const MAX_GRADE As Integer = 100

    ' Array for holding user inputs
    Dim inputListTxt As TextBox()

    ' Array for holding the reulting letter grade outputs
    Dim letterGradeList As Label()

    ' Variable containing the average grade for the semester
    Dim semAvg As Double = 0.00

    ' Counter for telling array what course the form is currently computing
    Dim numCounter As Integer = 0
#End Region

#Region "Functions and Subroutines"

    ' Function for calculating the letter grade from user input
    Function GetLetterGrade(numberGrade As Double) As String

        ' Returns ERROR if input isn't valid
        Dim letterGrade As String = "ERROR"

        ' Determines what grade correlates to what letter grade
        Select Case numberGrade
            Case 90 To 100
                letterGrade = "A+"
            Case 85 To 90
                letterGrade = "A"
            Case 80 To 85
                letterGrade = "A-"
            Case 75 To 80
                letterGrade = "B+"
            Case 70 To 75
                letterGrade = "B"
            Case 65 To 70
                letterGrade = "B-"
            Case 60 To 65
                letterGrade = "C"
            Case 55 To 60
                letterGrade = "D+"
            Case 50 To 55
                letterGrade = "D"
            Case Is < 50
                letterGrade = "F"
        End Select

        ' Sends result to requested area
        Return letterGrade
    End Function

    ' Function for validating user inputs
    Function ValidateInput(input As String) As Boolean

        ' Defaults validity to True until proven otherwise
        Dim isValid = True

        ' Determines whether user input is numeric
        If IsNumeric(input) Then

            ' Determines whether user input is within the alotted range
            If CDbl(input) >= MIN_GRADE And CDbl(input) <= MAX_GRADE Then

                ' User input is valid, clears any prior error messages
                lblResponseOutput.Text = ""

                ' Input is deemed to be outside of the range
            Else

                ' Displays range error message
                lblResponseOutput.Text = "ERROR Grade is outside applicable range. Please enter a score between 0 and 100."

                ' Flags input as invalid
                isValid = False
            End If

            ' Detects if textbox is empty. Ignores it
        ElseIf input = String.Empty Then

            ' Input is to be ignored. Clears prior error messages
            lblResponseOutput.Text = ""

            ' Flags input as invalid
            isValid = False

            ' Input is deemed to be non-numerical
        Else

            ' Displays non-numeric error message
            lblResponseOutput.Text = "ERROR Non-number was entered. Please enter a numeric value"

            ' Flags input as invalid
            isValid = False
        End If


        Return isValid
    End Function


#End Region

#Region "Event Handlers"

    ' Assigns values to arrays upon the form loading
    Private Sub SemesterAverageForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ' Assigns textbox values to the input list array
        inputListTxt = {txtCourseOneGrade, txtCourseTwoGrade, txtCourseThreeGrade, txtCourseFourGrade, txtCourseFiveGrade, txtCourseSixGrade}

        ' Assigns alotted grades to the letter grade outputs array
        letterGradeList = {lblCourseOneLetterGrade, lblCourseTwoLetterGrade, lblCourseThreeLetterGrade, lblCourseFourLetterGrade, lblCourseFiveLetterGrade, lblCourseSixLetterGrade}

    End Sub

    ' Calculates and displays letter grades for each course as the textbox loses focus
    Private Sub txtCourseGrade_LostFocus(sender As Object, e As EventArgs) Handles txtCourseOneGrade.LostFocus,
    txtCourseTwoGrade.LostFocus, txtCourseThreeGrade.LostFocus, txtCourseFourGrade.LostFocus, txtCourseFiveGrade.LostFocus,
    txtCourseSixGrade.LostFocus, txtCourseTwoGrade.Leave, txtCourseThreeGrade.Leave, txtCourseSixGrade.Leave, txtCourseOneGrade.Leave,
    txtCourseFourGrade.Leave, txtCourseFiveGrade.Leave

        ' Starts counter at 0
        numCounter = 0

        ' Procedure for each value in the user input array
        For Each textbox In inputListTxt

            ' Assigns trimmed textbox input to the input variable
            Dim input As String = Trim(inputListTxt(numCounter).Text)

            ' Input was validated correctly
            If ValidateInput(input) = True Then

                ' Assigns a letter grade
                letterGradeList(numCounter).Text = GetLetterGrade(CDbl(input))

                ' Counter counts up
                numCounter += 1

                ' Data came back as invalid
            Else

                ' Focuses on offending input
                inputListTxt(numCounter).Focus()

                ' Starts highlight process
                inputListTxt(numCounter).SelectionStart = 0

                ' Highlights entirity of offending input
                inputListTxt(numCounter).SelectionLength = Len(inputListTxt(numCounter).Text)
            End If
        Next

    End Sub

    ' Event for if Calculate button is pressed
    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click

        ' Resets counter
        numCounter = 0

        ' A loop that applies to each value in the input array
        For Each textbox In inputListTxt

            ' Trims input 
            Dim input As String = Trim(inputListTxt(numCounter).Text)

            ' Input is valid
            If ValidateInput(input) = True Then

                ' Restated for the final letterGrade that did not lose focus
                letterGradeList(numCounter).Text = GetLetterGrade(CDbl(input))

                ' Adds input into the semester average calculation
                semAvg += CDbl(input)

                ' Counts up
                numCounter += 1

                ' Invalid input found
            Else

                ' Similar to previous; Process for focusing/highlighting offending input
                inputListTxt(numCounter).Focus()
                inputListTxt(numCounter).SelectionStart = 0
                inputListTxt(numCounter).SelectionLength = Len(inputListTxt(numCounter).Text)

            End If

            ' All inputs are in, beginning of final calculations
        Next

        ' Divides sum of courses by the amount of courses inputted
        semAvg = semAvg / numCounter

        ' Rounds and displays resulting average
        lblSemesterAverage.Text = Math.Round(semAvg, 2).ToString

        ' Calculates letter grade for semester average
        lblSemesterLetterGrade.Text = GetLetterGrade(CDbl(lblSemesterAverage.Text))

        ' Disables Calculate button
        btnCalculate.Enabled = False

        ' Disables the input textboxes
        For Each textbox In inputListTxt
            textbox.ReadOnly = True
        Next

        ' Puts focus on the reset button
        btnReset.Focus()

    End Sub

    ' Event for if user presses Reset button
    Private Sub btnReset_Click(sender As Object, e As EventArgs) Handles btnReset.Click

        ' Clears out and re-enables textboxes
        For Each textbox In inputListTxt
            textbox.Text = String.Empty
            textbox.ReadOnly = False
        Next

        ' Clears out letter grades
        For Each label In letterGradeList
            label.Text = String.Empty
        Next

        ' Resets counter
        numCounter = 0

        ' Resets semester average
        semAvg = 0

        ' Clears any error messages
        lblResponseOutput.Text = String.Empty

        ' Clears semester average output
        lblSemesterAverage.Text = String.Empty

        ' Clears semester letter grade output
        lblSemesterLetterGrade.Text = String.Empty

        ' Re-enables Calculate button
        btnCalculate.Enabled = True

        ' Puts focus back on course one
        txtCourseOneGrade.Focus()

    End Sub

    ' Event for if user presses Exit button
    Private Sub btnExit_Click(sender As Object, e As EventArgs) Handles btnExit.Click

        ' Closes the form
        Close()
    End Sub

#End Region

End Class
