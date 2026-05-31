import { useState, useCallback } from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import type { ExecuteResponse } from '../lib/executeTypes';

async function readExecuteResponse(res: Response): Promise<ExecuteResponse> {
  const contentType = res.headers.get('content-type') || '';
  if (contentType.includes('application/json')) {
    try {
      return (await res.json()) as ExecuteResponse;
    } catch {
      // Fall through to text parsing.
    }
  }

  const text = (await res.text()).trim();

  if (res.status === 429) {
    return { error: 'Too many requests. Please wait and try again.' };
  }

  if (text) {
    return { error: text };
  }

  return { error: `Request failed (${res.status})` };
}

const useMuxExecutor = () => {
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const { siteConfig } = useDocusaurusContext();
  const customFields = siteConfig.customFields;
  const apiUrl =
    typeof customFields?.apiUrl === 'string'
      ? customFields.apiUrl
      : 'https://mux-lang-api.fly.dev';

  const executeCode = useCallback(async (source: string): Promise<ExecuteResponse> => {
    setLoading(true);
    setError(null);
    try {
      const res = await fetch(`${apiUrl}/api/compile`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code: source }),
      });

      const data = await readExecuteResponse(res);
      if (res.ok && !data.output && !data.error) {
        const msg = 'Server returned an unexpected response';
        setError(msg);
        return { error: msg };
      }
      if (!res.ok || data.error) {
        setError(data.error || 'Request failed');
      }
      return data;
    } catch (err: any) {
      const msg = err.message || 'Unknown error';
      setError(msg);
      return { error: msg };
    } finally {
      setLoading(false);
    }
  }, [apiUrl]);

  return {
    executeCode,
    loading,
    error,
  };
};

export default useMuxExecutor;
