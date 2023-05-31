
from fastapi import APIRouter, BackgroundTasks, Depends, File, UploadFile, Form, HTTPException, Response
import requests
router = APIRouter(prefix="/jobs",
                   tags=["jobs"],
                   responses={404: {"description": "Not found"}})

@router.get('/oncoplot')
async def get_job():
    print("job is fine !")
    test = ["12","32"]
    res = requests.post("http://127.0.0.1:11004/Read_exp_info", params={
    }).json()
    print(res)
    return res


@router.get('/query')
async def get_job(OR: str = None, Driver: str = None, Reporter: str = None):
    res = requests.post("http://127.0.0.1:11004/getQuery", params={
        "OR": OR,
        "Driver": Driver,
        "Reporter": Reporter
    }).json()
    print(res)
    return res


@router.get('/ORs')
async def get_job():
    res = requests.post("http://127.0.0.1:11004/getLevels", params={
        "column": "olfactory_receptor"
    }).json()
    return res

@router.get('/Drivers')
async def get_job():
    res = requests.post("http://127.0.0.1:11004/getLevels", params={
        "column": "promotor"
    }).json()
    return res

@router.get('/Reporters')
async def get_job():
    res = requests.post("http://127.0.0.1:11004/getLevels", params={
        "column": "reporter"
    }).json()
    return res

@router.get('/Odors')
async def get_job():
    res = requests.post("http://127.0.0.1:11004/getLevels", params={
        "column": "odor"
    }).json()
    return res

@router.get('/SameScalePlot')
async def get_job(OR: str = None, Driver: str = None, Reporter: str = None):
    res = requests.post("http://127.0.0.1:11004/getSamescale", params={
        "OR": OR,
        "Driver": Driver,
        "Reporter": Reporter
    }).json()
    return res

@router.get('/DataTable')
async def get_job(OR: str = None, Driver: str = None, Reporter: str = None, export: bool= True):
    res = requests.post("http://127.0.0.1:11004/getSamescale", params={
        "OR": OR,
        "Driver": Driver,
        "Reporter": Reporter,
        "export": export,
    }).json()
    return res

@router.get('/DataTableRaw')
async def get_job(OR: str = None, Driver: str = None, Reporter: str = None, export: bool= True, raw: bool=True ):
    res = requests.post("http://127.0.0.1:11004/getSamescale", params={
        "OR": OR,
        "Driver": Driver,
        "Reporter": Reporter,
        "export": export,
        "raw": raw,
    }).json()
    return res



