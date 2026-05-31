export interface ExecuteRequest {
  code: string;
}

export interface ExecuteResponse {
  output?: string;
  error?: string;
}
