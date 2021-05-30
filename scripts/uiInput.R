conditionalPanel(condition="input.tabselected==1",
                 fileInput("tr_data","Upload Training Dataset",placeholder = "No File Selected"),
                 fileInput("test_data","Upload Prediction Dataset",placeholder = "No File Selected"),
                 
)