 function addItem(){
        var li = document.createElement("li");  
        var input = document.getElementById("ingredient-form");
        li.innerHTML = input.value;
        input.value = "";

        document.getElementById("ingredients").appendChild(li);
    }
