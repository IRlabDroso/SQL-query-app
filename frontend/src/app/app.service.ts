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
            Reporter: any| null=null,
            Odorant: any| null=null,
            Dilution: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/query"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      "Odorant": Odorant,
      "Dilution": Dilution
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

   getOdorants(OR: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/Odorants"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR
      }})
   };

   getDilutions(OR: any| null=null,
                Odorant: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/Dilutions"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Odorant": Odorant
      }})
   };

   getSameScalePlot(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null,
                    Odorant: any| null=null,
                    Dilution: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/SameScalePlot"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      "Odorant": Odorant,
      "Dilution": Dilution
      }})
   };

   getDataTable(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null,
                    Odorant: any| null=null,
                    Dilution: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/DataTable"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      "Odorant": Odorant,
      "Dilution": Dilution
      }})
   };

   getDataTableRaw(OR: any| null=null,
                    Driver: any| null=null,
                    Reporter: any| null=null,
                    Odorant: any| null=null,
                    Dilution: any| null=null): Observable<any> {
      const url = environment.back_end + "jobs/DataTableRaw"
      return this.http.get(url, {responseType: "json", params:{
      "OR": OR,
      "Driver": Driver,
      "Reporter": Reporter,
      "Odorant": Odorant,
      "Dilution": Dilution
      }})
   };


}
