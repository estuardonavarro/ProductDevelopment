# Desarrollado por: Javier Navarro


from typing import Dict, Optional
from fastapi import FastAPI
from enum import Enum
from pydantic import BaseModel
from fastapi.responses import ORJSONResponse, UJSONResponse, HTMLResponse, StreamingResponse
import pandas as pd
import io

app = FastAPI()


class RoleName(str, Enum):
    admin = 'Admin'
    writer = 'Writer'
    reader = 'Reader'


class OperationName(str, Enum):
    summ = 'Suma'
    rest = 'Resta'
    mult = 'Multiplicación'
    div = 'División'


class Item(BaseModel):
    name: str
    description: Optional[str] = None
    price: float
    tax: Optional[float] = None


class Operations(BaseModel):
    Number1: float
    Number2: float


@app.get('/')
def root():
    return {'message': 'Hello World, from Galileo Master, Section V'}

@app.get('/users/me')
def read_current_user():
    return {'user_id': 'The current logged user'}


@app.get('/users/{user_id}')
def read_user(user_id: str):
    return {'user_id': user_id}


@app.get('/roles/{role_name}')
def get_role_permissions(role_name: RoleName):
    if role_name == RoleName.admin:
        return {'role_name': role_name, 'permisisions': 'Full acces'}

    if role_name == RoleName.writer:
        return {'role_name': role_name, 'permisisions': 'Write acces'}

    return {'role_name': role_name, 'permisisions': 'Read acces only'}


fake_items_db = [{'item_name': 'uno'}, {'item_name': 'dos'}, {'item_name': 'tres'}]


@app.get('/items/')
def read_items(skip: int = 0, limit: int = 10):
    return fake_items_db[skip: skip + limit]


@app.get('/items/{item_id}')
def read_item_query(item_id: int, query: Optional[str] = None):
    message = {'item_id': item_id}
    if query:
        message['query'] = query

    return message


@app.get('/users/{user_id}/items/{item_id}')
def read_user_item(user_id: int, item_id: int, query: Optional[str] = None, describe: bool = False):
    item = {'item_id': item_id, 'owner_id': user_id}

    if query:
        item['query'] = query

    if describe:
        item['description'] = 'This is a long description for the item'

    return item


@app.post('/items/')
def create_item(item: Item):
    return {
        'message': 'The item was successfully created',
        'item': item.dict()
    }


@app.put('/items/{item_id}')
def update_item(item_id: int, item: Item):
    if item.tax == 0 or item.tax is None:
        item.tax = item.price * 0.12

    return {
        'message': 'The item was updated',
        'item_id': item_id,
        'item': item.dict()
    }


# tipos de respuestas
@app.get('/itemsall', response_class=UJSONResponse)
def read_long_json():
    return [{'item_id': 'item'}, {'item_id': 'item'}, {'item_id': 'item'},
            {'item_id': 'item'}, {'item_id': 'item'}, {'item_id': 'item'}]


@app.get('/html', response_class=HTMLResponse)
def read_html():
    return '''

        <html>
            <head>
                <title>Some HTML in here</title> 
            </head>
            
        </html>
        
    '''


@app.get("/csv")
def get_csv():
    df = pd.DataFrame({"Column A": [1, 2], "Column B": [3, 4]})

    stream = io.StringIO()

    df.to_csv(stream, index=False)

    response = StreamingResponse(iter([stream.getvalue()]), media_type='text/csv')

    response.headers['Content-Disposition'] = "attachment; filename=my_awesome_report.csv"

    return response


# RESOLUCIÓN DE TAREA:

@app.post('/operations/{op_name}')
def aritmethic_operation(numbers: Operations, op_name: OperationName):
    if op_name == "Suma":
        return {
            'Operation': op_name,
            'Number 1': numbers.Number1,
            'Number 2': numbers.Number2,
            'Result': numbers.Number1 + numbers.Number2,
        }
    elif op_name == "Resta":
        return {
            'Operation': op_name,
            'Number 1': numbers.Number1,
            'Number 2': numbers.Number2,
            'Result': numbers.Number1 - numbers.Number2,
        }
    elif op_name == "Multiplicación":
        return {
            'Operation': op_name,
            'Number 1': numbers.Number1,
            'Number 2': numbers.Number2,
            'Result': numbers.Number1 * numbers.Number2,
        }
    elif op_name == "División":
        return {
            'Operation': op_name,
            'Number 1': numbers.Number1,
            'Number 2': numbers.Number2,
            'Result': numbers.Number1 / numbers.Number2,
        }


@app.get('/operations/{op_name}')
def aritmethic_operation_get(op_name: OperationName, number1: float, number2: float):
    if op_name == "Suma":
        return {
            'Operation': op_name,
            'Number 1': number1,
            'Number 2': number2,
            'Result': number1 + number2,
        }
    elif op_name == "Resta":
        return {
            'Operation': op_name,
            'Number 1': number1,
            'Number 2': number2,
            'Result': number1 - number2,
        }
    elif op_name == "Multiplicación":
        return {
            'Operation': op_name,
            'Number 1': number1,
            'Number 2': number2,
            'Result': number1 * number2,
        }
    elif op_name == "División":
        return {
            'Operation': op_name,
            'Number 1': number1,
            'Number 2': number2,
            'Result': number1 / number2,
        }
