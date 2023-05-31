import { Injectable } from '@angular/core';
import {BehaviorSubject} from "rxjs";
import {environment} from '../environments/environment';
import {Observable} from 'rxjs';
import {HttpClient, HttpHeaders, HttpParams} from '@angular/common/http';
import {FormControl} from "@angular/forms";

@Injectable({
  providedIn: 'root'
})

export class ApiService {

   constructor(private http: HttpClient) { }

   getOncoplot(): Observable<any> {
      const url = environment.back_end + "jobs/oncoplot"
      return this.http.get(url, {responseType: "json"})
   };

   getQuery(OR: any| null=null,
            Driver: any| null=null,
            Reporter: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/query"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      }})
   }

   getORs(): Observable<any> {
      const url = environment.back_end + "jobs/ORs"
      return this.http.get(url, {responseType: "json"})
   };

   getDrivers(): Observable<any> {
      const url = environment.back_end + "jobs/Drivers"
      return this.http.get(url, {responseType: "json"})
   };

   getReporters(): Observable<any> {
      const url = environment.back_end + "jobs/Reporters"
      return this.http.get(url, {responseType: "json"})
   };
   getOdors(): Observable<any> {
      const url = environment.back_end + "jobs/Odors"
      return this.http.get(url, {responseType: "json"})
   };

   getSameScalePlot(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/SameScalePlot"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      }})
   };

   getDataTable(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/DataTable"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      }})
   };

   getDataTableRaw(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/DataTableRaw"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      }})
   };


}
