import { useState, useCallback } from 'react';
import type { ExecuteRequest, ExecuteResponse } from '../lib/executeTypes';

type Executor = (source: string) => Promise<ExecuteResponse>;

const useMuxExecutor = () => {
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);

  // Mock execution function
  const executeWithMock = useCallback(async (source: string): Promise<ExecuteResponse> => {
    // Simulate network request delay
    await new Promise(resolve => setTimeout(resolve, 800));
    
    // Simple mock: if the code contains the word "error", return an error, else return a fixed output.
    if (source.includes('error')) {
      return { output: '', error: 'Mock error: intentional error for testing' };
    }
    
    // For demonstration, we'll return a fixed output that includes the input code
    const output = `Execution successful:\n${source}`;
    return { output, error: undefined };
  }, []);

  // Main execute function that uses the mock
  const executeCode = useCallback(async (source: string): Promise<ExecuteResponse> => {
    setLoading(true);
    setError(null);
    try {
      return await executeWithMock(source);
    } catch (err: any) {
      setError(err.message || 'Unknown error');
      return { output: '', error: err.message || 'Unknown error' };
    } finally {
      setLoading(false);
    }
  }, [executeWithMock]);

  return {
    executeCode,
    wasmLoaded: false, // We're not using WASM in the stub
    loading,
    error,
  };
};

export default useMuxExecutor;
